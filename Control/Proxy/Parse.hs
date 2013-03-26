{-# LANGUAGE DeriveDataTypeable #-}

{-| This module defines the core machinery for non-backtracking parsers

    You can directly interleave non-backtracking parsing code with other @pipes@
    code, unlike backtracking parsers, which you must first 'commit' before
    embedding within a pipe.
-}
module Control.Proxy.Parse (
    -- * Non-backtracking parser
    ParseP,

    -- * Parse exception
    ParseFailure(..),

    -- * Single-element parsers
    draw,
    skip,
    drawIf,
    skipIf,

    -- * Efficient bulk parsers
    drawN,
    skipN,
    drawWhile,
{-
    skipWhile,
    drawAll,
    skipAll,

    -- * Pushback
    unDraw,

    -- * Fail-safe parsers
    drawMay,
    peek,

    -- * Delimited parsing
    parseN,
    parseWhile,

    -- * End of input
    endOfInS.put,
    isEndOfInS.put,

    -- * Error messages
    parseError,
    modifyError,
    setError,
    (<?>),

-}
    -- * Run functions
    evalParseP,
    evalParseK,

    -- * End of input utilities
    only,
    onlyK,
{-
    just,
    justK,

    -- * Re-exports
    -- $reexport
    module Control.Applicative,
    module Control.Monad,
    module Control.Proxy.Trans.Either
-}
    ) where

import Control.Applicative (
    Applicative(pure, (<*>), (<*), (*>)), Alternative(empty, (<|>), some, many))
import Control.Exception (SomeException, Exception, toException)
import qualified Control.Proxy as P
import Control.Proxy ((>>~))
import Control.Proxy.Parse.Internal (ParseP(ParseP, unParseP), get, put, throw)
import Control.Proxy.Trans.Codensity (runCodensityP)
import qualified Control.Proxy.Trans.Either as E
import qualified Control.Proxy.Trans.State as S
import Data.Maybe (isJust, isNothing)
import Data.Typeable (Typeable)

-- For re-exports
import Control.Applicative ((<$>), (<$), (<**>), optional)
import Control.Monad (
    replicateM, replicateM_, MonadPlus(mzero, mplus), msum, mfilter, guard )

-- | Parsing failed.  The 'String' describes the nature of the parse failure.
newtype ParseFailure = ParseFailure String deriving (Show, Typeable)

instance Exception ParseFailure

-- | Request a single element
draw :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) b m a
draw = do
    s <- get
    case s of
        []     -> do
            ma <- P.request ()
            case ma of
                Nothing -> err
                Just a  -> return a
        ma:mas -> case ma of
            Nothing -> err
            Just a  -> do
                put mas
                return a
  where
    err = throw (ParseFailure "draw: End of input")
{-# INLINABLE draw #-}

-- | Skip a single element
skip :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) b m ()
skip = do
    s <- get
    case s of
        []     -> do
            ma <- P.request ()
            case ma of
                Nothing -> err
                Just _  -> return ()
        ma:mas -> case ma of
            Nothing -> err
            Just _  -> put mas
  where
    err = throw (ParseFailure "skip: End of input")
{-# INLINABLE skip #-}

-- | Request a single element that must satisfy the predicate
drawIf
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) b m a
drawIf pred = do
    s <- get
    case s of
        []     -> do
            ma <- P.request ()
            case ma of
                Nothing -> err0
                Just a  -> if (pred a) then return a else err1
        ma:mas -> do
            ma <- P.request ()
            case ma of
                Nothing -> err0
                Just a  -> if (pred a)
                    then do
                        put mas
                        return a
                    else err1
  where
    err0 = throw (ParseFailure "drawIf: End of input")
    err1 = throw (ParseFailure "drawIf: Element failed predicate")
{-# INLINABLE drawIf #-}

-- | Skip a single element that must satisfy the predicate
skipIf
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) b m ()
skipIf pred = do
    s <- get
    case s of
        []     -> do
            ma <- P.request ()
            case ma of
                Nothing -> err0
                Just a  -> if (pred a) then return () else err1
        ma:mas -> case ma of
            Nothing -> err0
            Just a  -> put mas
  where
    err0 = throw (ParseFailure "skipIf: End of input")
    err1 = throw (ParseFailure "skipIf: Elemented failed predicate")
{-# INLINABLE skipIf #-}

-- | Request a fixed number of elements
drawN :: (Monad m, P.Proxy p) => Int -> P.Pipe (ParseP a p) (Maybe a) b m [a]
drawN n0 = go0 id n0
  where
    go0 diffAs n = if (n > 0)
        then do
            s <- get
            case s of
                []     -> go1 diffAs n
                ma:mas -> case ma of
                    Nothing -> err n
                    Just a  -> do
                        put mas
                        go0 (diffAs . (a:)) $! n - 1
        else return (diffAs [])
    go1 diffAs n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> err n
                Just a  -> go1 (diffAs . (a:)) $! n - 1
         else return (diffAs [])
    err nLeft = throw (ParseFailure (
        "drawN " ++ show n0 ++ ": Found only " ++ show (n0 - nLeft)
     ++ " elements" ))
{-# INLINABLE drawN #-}

{-| Skip a fixed number of elements

    Faster than 'drawN' if you don't need the input
-}
skipN :: (Monad m, P.Proxy p) => Int -> P.Pipe (ParseP a p) (Maybe a) b m ()
skipN n0 = go0 n0
  where
    go0 n = if (n > 0)
        then do
            s <- get
            case s of
                []     -> go1 n
                ma:mas -> case ma of
                    Nothing -> err n
                    Just _  -> go0 $! n - 1
        else return ()
    go1 n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> err n
                Just _  -> go1 $! n - 1
        else return ()
    err nLeft = throw (ParseFailure (
        "skipN " ++ show n0 ++ ": Found only " ++ show (n0 - nLeft)
     ++ " elements" ))
{-# INLINABLE skipN #-}

-- | Request as many consecutive elements satisfying a predicate as possible
drawWhile
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) b m [a]
drawWhile pred = go0 id
  where
    go0 diffAs = do
        s <- get
        case s of
            []     -> go1 diffAs
            ma:mas -> case ma of
                Nothing -> return (diffAs [])
                Just a  -> if (pred a)
                    then do
                        put mas
                        go0 (diffAs . (a:))
                    else return (diffAs [])
    go1 diffAs = do
        ma <- P.request ()
        case ma of
            Nothing -> do
                put [ma]
                return (diffAs [])
            Just a  -> if (pred a)
                then go1 (diffAs . (a:))
                else do
                    put [ma]
                    return (diffAs [])
{-
drawWhile pred = ParseP (S.StateP (\s0 -> E.EitherP (go0 id s0)))
  where
    go0 diffAs s = case s of
        [] -> go1 diffAs
        ma:<mas  -> case ma of
            Nothing -> return (Right (diffAs [], s))
            Just a  ->
                if (pred a)
                    then go0 (diffAs . (a:)) mas
                    else return (Right (diffAs [], s))
    go1 diffAs = do
        ma <- P.request ()
        case ma of
            Nothing -> return (Right (diffAs [], S.singleton ma))
            Just a  ->
                if (pred a)
                    then go1 (diffAs . (a:))
                    else return (Right (diffAs [], S.singleton ma))
-}
{-# INLINABLE drawWhile #-}

{-
{-| Request as many consecutive elements satisfying a predicate as possible

    Faster than 'drawWhile' if you don't need the input
-}
skipWhile
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Consumer (ParseP a p) (Maybe a) m ()
skipWhile pred = ParseP (StateP (\s0 -> E.EitherP (go0 s0)))
  where
    go0 s = case S.viewl s of
        S.EmptyL -> go1
        ma:<mas  -> case ma of
            Nothing -> return (Right ((), s))
            Just a  ->
                if (pred a)
                    then go0 mas
                    else return (Right ((), s))
    go1 = do
        ma <- P.request ()
        case ma of
            Nothing -> return (Right ((), S.singleton ma))
            Just a  ->
                if (pred a)
                    then go1
                    else return (Right ((), S.singleton ma))
{-# INLINABLE skipWhile #-}

-- | Request the rest of the input
drawAll
    :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m [a]
drawAll = ParseP (StateP (\s0 -> E.EitherP (go0 id s0))) where
    go0 diffAs s = case S.viewl s of
        S.EmptyL -> go1 diffAs
        ma:<mas  -> case ma of
            Nothing -> return (Right (diffAs [], s))
            Just a  -> go0 (diffAs . (a:)) mas
    go1 diffAs = do
        ma <- P.request ()
        case ma of
            Nothing -> return (Right (diffAs [], S.singleton ma))
            Just a  -> go1 (diffAs . (a:))
{-# INLINABLE drawAll #-}

{-| Skip the rest of the input

    Faster than 'drawAll' if you don't need the input
-}
skipAll
    :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m ()
skipAll = ParseP (StateP (\s0 -> E.EitherP (go0 s0))) where
    go0 s = case S.viewl s of
        S.EmptyL -> go1
        ma:<mas  -> case ma of
            Nothing -> return (Right ((), s))
            Just a  -> go0 mas
    go1 = do
        ma <- P.request ()
        case ma of
            Nothing -> return (Right ((), S.singleton ma))
            Just a  -> go1
{-# INLINABLE skipAll #-}

-- | Push a single element into the leftover buffer
unDraw :: (Monad m, P.Proxy p) => a -> P.Consumer (ParseP a p) (Maybe a) m ()
unDraw a = ParseP (StateP (\s -> E.EitherP (return (Right ((), Just a <| s)))))
{-# INLINABLE unDraw #-}

-- | Request 'Just' one element or 'Nothing' if at end of input
drawMay :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m (Maybe a)
drawMay = ParseP (StateP (\s -> E.EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (Right (ma, case ma of
            Nothing -> S.singleton ma
            _       -> S.empty ))
    ma:<mas  -> return (Right (ma, case ma of
        Nothing -> s
        _       -> mas )) )))
{-# INLINABLE drawMay #-}

{-| Look ahead one element without consuming it

    Faster than 'drawMay' followed by 'unDraw'
-}
peek :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m (Maybe a)
peek = ParseP (StateP (\s -> E.EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (Right (ma, S.singleton ma))
    ma:<mas  -> return (Right (ma, s)) )))
{-# INLINABLE peek #-}

{- NOTE: parseN and parseWhile work by inserting a fake end-of-input marker
         (i.e. Nothing) using a 'block' function and then removing it after the
         parser completes using an 'unblock' function.  This trick does not
         work if the user's parser can consume the end-of-input marker, so I
         cannot expose any parsing primitives that consume this marker.
-}

-- | @(parseN n p)@ restricts the parser @p@ to only use the next @n@ elements
parseN
    :: (Monad m, P.Proxy p)
    => Int
    -> P.Consumer (ParseP a p) (Maybe a) m r
    -> P.Consumer (ParseP a p) (Maybe a) m r
parseN n0 p = do
    block
    r <- p
    unBlock
    return r
  where
    block = ParseP (StateP (\s -> E.EitherP (do
        case (S.findIndexL isNothing s) of
            Nothing -> go s (n0 - S.length s)
            Just n
                | n < n0    -> return (Left (err (S.length s)))
                | otherwise -> do
                    let (prefix, suffix) = S.splitAt n0 s
                    return (Right ((), (prefix |> Nothing) >< suffix)) )))
    go s n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> return (Left (err n))
                _       -> go (s |> ma) $! n - 1
        else return (Right ((), s |> Nothing))
    unBlock = ParseP (StateP (\s -> E.EitherP (do
        let (prefix, suffix) = S.spanl isJust s
        return (Right ((), prefix >< (case S.viewl suffix of
            S.EmptyL -> suffix
            _:<mas   -> mas ))) )))
    err nLeft = "parseN " ++ show n0 ++ ": Found only " ++ show (n0 - nLeft)
             ++ " elements"
{-# INLINABLE parseN #-}

{-| @(parseWhile pred p)@ restricts the parser @p@ to only use input that
    satisfies the predicate @pred@. -}
parseWhile
    :: (Monad m, P.Proxy p)
    => (a -> Bool)
    -> P.Consumer (ParseP a p) (Maybe a) m r
    -> P.Consumer (ParseP a p) (Maybe a) m r
parseWhile pred p = do
    block
    r <- p
    unBlock
    return r
  where
    block = ParseP (StateP (\s -> E.EitherP (do
        case (S.findIndexL (maybe True (not . pred)) s) of
            Nothing -> go s
            Just n  -> do
                let (prefix, suffix) = S.splitAt n s
                return (Right ((), (prefix |> Nothing) >< suffix)) )))
    go s = do
        ma <- P.request ()
        case ma of
            Nothing -> return (Right ((), s |> Nothing |> ma))
            Just a  ->
                if (pred a)
                    then go (s |> ma)
                    else return (Right ((), s |> Nothing |> ma))
    unBlock = ParseP (StateP (\s -> E.EitherP (do
        let (prefix, suffix) = S.spanl isJust s
        return (Right ((), prefix >< (case S.viewl suffix of
            S.EmptyL -> suffix
            _:<mas   -> mas ))) )))
{-# INLINABLE parseWhile #-}

-- | Match end of input without consuming it
endOfInS.put :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m ()
endOfInS.put = ParseP (StateP (\s -> E.EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (case ma of
            Nothing -> Right ((), S.singleton ma)
            Just a  -> Left "endOfInS.put: Not end of input" )
    ma:<mas  -> return (case ma of
        Nothing -> Right ((), s)
        Just a  -> Left "endOfInS.put: Not end of input" ) )))
{-# INLINABLE endOfInS.put #-}

-- | Return whether cursor is at end of input
isEndOfInS.put :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m Bool
isEndOfInS.put = ParseP (StateP (\s -> E.EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (Right (case ma of
            Nothing -> True
            _       -> False , S.singleton ma ))
    ma:<mas  -> return (Right (case ma of
        Nothing -> True
        _       -> False , mas )) )))
{-# INLINABLE isEndOfInS.put #-}

-- | Emit a diagnostic message and abort parsing
parseError
    :: (Monad m, P.Proxy p) => String -> P.Consumer (ParseP a p) (Maybe a) m r
parseError str = ParseP (StateP (\_ -> E.EitherP (return (Left str))))
{-# INLINABLE parseError #-}

-- | Set a new default error message for a parser
modifyError
    :: (Monad m, P.Proxy p)
    => (String -> String)  -- ^ Function to modify error message
    -> P.Consumer (ParseP a p) (Maybe a) m r  -- ^ Parser to modify
    -> P.Consumer (ParseP a p) (Maybe a) m r
modifyError f p = ParseP (StateP (\s -> E.EitherP (do
    e <- runEitherP (unStateP (unParseP p) s)
    return (case e of
        Left  l -> Left (f l)
        Right r -> Right r ) )))
{-# INLINABLE modifyError #-}

-- | Set a new default error message for a parser
setError
    :: (Monad m, P.Proxy p)
    => String                                 -- ^ New default error message
    -> P.Consumer (ParseP a p) (Maybe a) m r  -- ^ Parser to modify
    -> P.Consumer (ParseP a p) (Maybe a) m r
setError str = modifyError (\_ -> str)
{-# INLINABLE setError #-}

-- | Infix version of 'setError' with arguments flipped
(<?>)
    :: (Monad m, P.Proxy p)
    => P.Consumer (ParseP a p) (Maybe a) m r  -- ^ Parser to modify
    -> String                                 -- ^ New default error message
    -> P.Consumer (ParseP a p) (Maybe a) m r
p <?> str= setError str p
{-# INLINABLE (<?>) #-}

infixl 0 <?>
-}

{-| Evaluate a non-backtracking parser, returning the result or failing with a
    'ParseFailure' exception.
-}
evalParseP
    :: (Monad m, P.Proxy p)
    => ParseP i p a' a b' b m r
    -> E.EitherP SomeException p a' a b' b m r
evalParseP p = S.evalStateP [] (unParseP p)

{-| Evaluate a non-backtracking parser \'@K@\'leisli arrow, returning the result
    or failing with a 'ParseFailure' exception.
-}
evalParseK
    :: (Monad m, P.Proxy p)
    => (q -> ParseP i p a' a b' b m r)
    -> (q -> E.EitherP SomeException p a' a b' b m r)
evalParseK k q = evalParseP (k q)

{- $reexport
    @Control.Applicative@ exports useful combinators for 'Functor',
    'Applicative', and 'Alternative', like 'many', ('<|>'), and 'optional'.

    @Control.Monad@ exports useful combinators for 'Monad' and 'MonadPlus',
    like 'replicateM', 'msum', and 'mfilter'.

    @Control.Monad.Trans.Either@ exports run functions for the 'E.EitherP' proxy
    transformer.
-}

-- | Wrap a proxy's outS.put in 'Just' and finish with a 'Nothing'
only :: (Monad m, P.Proxy p) => p a' a b' b m r -> p a' a b' (Maybe b) m r
only p = P.runIdentityP (do
    r <- P.IdentityP p >>~ wrap
    P.respond Nothing
    return r )
  where
    wrap a = do
        a' <- P.respond (Just a)
        a2 <- P.request a'
        wrap a2
{-# INLINABLE only #-}

{-| Wrap a proxy \'@K@\'leisli arrow's outS.put in 'Just' and finish with a
    'Nothing'
-}
onlyK
    :: (Monad m, P.Proxy p)
    => (q -> p a' a b' b m r) -> (q -> p a' a b' (Maybe b) m r)
onlyK k q = only (k q)
{-# INLINABLE onlyK #-}

{-
{-| Upgrade a proxy to work with 'Maybe's

    The upgraded proxy handles 'Just's and auto-forwards 'Nothing's
-}
just :: (Monad m, P.ListT p) => p x a x b m r -> p x (Maybe a) x (Maybe b) m r
just p = P.runIdentityP (up >\\ (P.IdentityP p //> dn))
  where
    dn b = P.respond (Just b)
    up x = do
        ma <- P.request x
        case ma of
            Nothing -> do
                x2 <- P.respond Nothing
                up x2
            Just a  -> return a
{-# INLINABLE just #-}

{-| Upgrade a proxy \'@K@\'leisli arrow to work with 'Maybe's

    The upgraded proxy handles 'Just's and auto-forwards 'Nothing's
-}
justK
    :: (Monad m, P.ListT p)
    => (q -> p x a x b m r) -> (q -> p x (Maybe a) x (Maybe b) m r)
justK k q = just (k q)
{-# INLINABLE justK #-}
-}
