{-# LANGUAGE DeriveDataTypeable #-}

{-| This module defines the core machinery for non-backtracking parsers

    You can directly interleave non-backtracking parsing code with other @pipes@
    code, unlike backtracking parsers, which you must first 'commit' before
    embedding within a pipe.
-}
module Control.Proxy.Parse.Commit (
    -- * Non-backtracking parser
    ParseP,

    -- * Single-element parsers
    draw,
    skip,
    drawIf,
    skipIf,

    -- * Efficient bulk parsers
    drawN,
    skipN,
    drawWhile,
    skipWhile,
    drawAll,
    skipAll,

    -- * Pushback
    unDraw,
    peek,

    -- * End of input
    endOfInput,
    nextInput,

    -- * Diagnostic messages
    parseError,
    modifyError,
    setError,
    (<?>),

    -- * Run functions
    evalParseP,
    evalParseK,

    -- * End of input utilities
    only,
    onlyK,
    just,

    -- * Re-exports
    -- $reexport
    module Control.Applicative,
    module Control.Monad,
    module Control.Proxy.Trans.Maybe
    ) where

import Control.Applicative (
    Applicative(pure, (<*>), (<*), (*>)), Alternative(empty, (<|>), some, many))
import Control.Monad (forever)
import Control.Exception (SomeException, Exception, toException)
import qualified Control.Proxy as P
import Control.Proxy ((>>~))
import Control.Proxy.Parse.Internal (
    ParseP(ParseP, unParseP), only, onlyK, just)
import Control.Proxy.Trans.Codensity (runCodensityP)
import Control.Proxy.Trans.Either (EitherP(EitherP, runEitherP), fmapL)
import Control.Proxy.Trans.State (StateP(StateP, unStateP), evalStateP)
import Control.Proxy.Trans.Maybe (MaybeP(MaybeP, runMaybeP), runMaybeK)
import qualified Data.Sequence as S
import Data.Sequence (ViewL((:<)), (<|))
import Data.Typeable (Typeable)

-- For re-exports
import Control.Applicative ((<$>), (<$), (<**>), optional)
import Control.Monad (
    replicateM_, MonadPlus(mzero, mplus), msum, mfilter, guard )

-- | Request a single element
draw :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m a
draw = ParseP (StateP (\s -> EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (case ma of
            Nothing -> Left "draw: End of input"
            Just a  -> Right (a, s) )
    ma:<mas  -> return (case ma of
        Nothing -> Left "draw: End of input"
        Just a  -> Right (a, mas) ) )))
{-# INLINABLE draw #-}

-- | Skip a single element
skip :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m ()
skip = ParseP (StateP (\s -> EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (case ma of
            Nothing -> Left "skip: End of input"
            Just _  -> Right ((), s) )
    ma:<mas  -> return (case ma of
        Nothing -> Left "skip: End of input"
        Just _  -> return ((), mas) ) )))
{-# INLINABLE skip #-}

-- | Request a single element that must satisfy the predicate
drawIf
 :: (Monad m, P.Proxy p)
 => (a -> Bool) -> P.Consumer (ParseP a p) (Maybe a) m a
drawIf pred = ParseP (StateP (\s -> EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (case ma of
            Nothing -> Left "drawIf: End of input"
            Just a  ->
                if (pred a)
                    then Right (a, s)
                    else Left "drawIf: Element failed predicate" )
    ma:<mas  -> return (case ma of
        Nothing -> Left "drawIf: End of input"
        Just a  ->
            if (pred a)
                then Right (a, mas)
                else Left "drawIf: Element failed predicate" ) )))
{-# INLINABLE drawIf #-}

-- | Skip a single element that must satisfy the predicate
skipIf
 :: (Monad m, P.Proxy p)
 => (a -> Bool) -> P.Consumer (ParseP a p) (Maybe a) m ()
skipIf pred = ParseP (StateP (\s -> EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (case ma of
            Nothing -> Left "skipIf: End of input"
            Just a  ->
                if (pred a)
                then Right ((), s)
                else Left "skipIf: Element failed predicate" )
    ma:<mas  -> return (case ma of
        Nothing -> Left "skipIf: End of input"
        Just a  ->
            if (pred a)
                then Right ((), mas)
                else Left "skipIf: Element failed predicate" ) )))
{-# INLINABLE skipIf #-}

-- | Request a fixed number of elements
drawN
 :: (Monad m, P.Proxy p) => Int -> P.Consumer (ParseP a p) (Maybe a) m [a]
drawN n0 = ParseP (StateP (\s0 -> EitherP (go0 id s0 n0))) where
    go0 diffAs s n = if (n > 0)
        then case S.viewl s of
            S.EmptyL -> go1 diffAs n
            ma:<mas  -> case ma of
                Nothing -> return (Left (err n))
                Just a  -> go0 (diffAs . (a:)) mas $! (n - 1)
        else return (Right (diffAs [], s))
    go1 diffAs n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> return (Left (err n))
                Just a  -> go1 (diffAs . (a:)) $! (n - 1)
        else return (Right (diffAs [], S.empty))
    err nLeft = "drawN " ++ show n0 ++ ": Found only " ++ show (n0 - nLeft)
             ++ " elements"
{-# INLINABLE drawN #-}

{-| Skip a fixed number of elements

    Faster than 'drawN' if you don't need the input
-}
skipN
 :: (Monad m, P.Proxy p) => Int -> P.Consumer (ParseP a p) (Maybe a) m ()
skipN n0 = ParseP (StateP (\s0 -> EitherP (go0 s0 n0))) where
    go0 s n = if (n > 0)
        then case S.viewl s of
            S.EmptyL -> go1 n
            ma:<mas  -> case ma of
                Nothing -> return (Left (err n))
                Just _  -> go0 mas $! (n - 1)
        else return (Right ((), s))
    go1 n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> return (Left (err n))
                Just _  -> go1 $! (n - 1)
        else return (Right ((), S.empty))
    err nLeft = "skipN " ++ show n0 ++ ": Found only " ++ show (n0 - nLeft)
             ++ " elements"
{-# INLINABLE skipN #-}

-- | Request as many consecutive elements satisfying a predicate as possible
drawWhile
 :: (Monad m, P.Proxy p)
 => (a -> Bool) -> P.Consumer (ParseP a p) (Maybe a) m [a]
drawWhile pred = ParseP (StateP (\s0 -> EitherP (go0 id s0)))
  where
    go0 diffAs s = case S.viewl s of
        S.EmptyL -> go1 diffAs
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
{-# INLINABLE drawWhile #-}

{-| Request as many consecutive elements satisfying a predicate as possible

    Faster than 'drawWhile' if you don't need the input
-}
skipWhile
 :: (Monad m, P.Proxy p)
 => (a -> Bool) -> P.Consumer (ParseP a p) (Maybe a) m ()
skipWhile pred = ParseP (StateP (\s0 -> EitherP (go0 s0)))
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
drawAll = ParseP (StateP (\s0 -> EitherP (go0 id s0))) where
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
skipAll = ParseP (StateP (\s0 -> EitherP (go0 s0))) where
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
unDraw a = ParseP (StateP (\s -> EitherP (return (Right ((), Just a <| s)))))
{-# INLINABLE unDraw #-}

{-| Look ahead one element without consuming it

    Faster than 'drawMay' followed by 'unDraw'
-}
peek :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m (Maybe a)
peek = ParseP (StateP (\s -> EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (Right (ma, S.singleton ma))
    ma:<mas  -> return (Right (ma, s)) )))
{-# INLINABLE peek #-}

-- | Match end of input without consuming it
endOfInput :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m ()
endOfInput = ParseP (StateP (\s -> EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (case ma of
            Nothing -> Right ((), S.singleton ma)
            Just a  -> Left "endOfInput: Not end of input" )
    ma:<mas  -> return (case ma of
        Nothing -> Right ((), s)
        Just a  -> Left "endOfInput: Not end of input" ) )))
{-# INLINABLE endOfInput #-}

{-| Consume the end of input token (i.e. 'Nothing'), advancing to the next input

    This is the only primitive that consumes the end of input token.
-}
nextInput :: (Monad m, P.Proxy p) => P.Consumer (ParseP a p) (Maybe a) m ()
nextInput = ParseP (StateP (\s -> EitherP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        return (case ma of
            Nothing -> Right ((), S.empty)
            Just a  -> Left "nextInput: Not end of input" )
    ma:<mas  -> return (case ma of
        Nothing -> Right ((), mas)
        Just a  -> Left "nextInput: Not end of input" ) )))
{-# INLINABLE nextInput #-}

-- | Emit a diagnostic message and abort parsing
parseError
 :: (Monad m, P.Proxy p) => String -> P.Consumer (ParseP a p) (Maybe a) m r
parseError str = ParseP (StateP (\_ -> EitherP (return (Left str))))
{-# INLINABLE parseError #-}

-- | Set a new default error message for a parser
modifyError
 :: (Monad m, P.Proxy p)
 => (String -> String)                     -- ^ Function to modify error message
 -> P.Consumer (ParseP a p) (Maybe a) m r  -- ^ Parser to modify
 -> P.Consumer (ParseP a p) (Maybe a) m r
modifyError f p = ParseP (StateP (\s -> EitherP (do
    e <- runEitherP (unStateP (unParseP p) s)
    return (case e of
        Left  l -> Left (f l)
        Right r -> Right r ) )))

-- | Set a new default error message for a parser
setError
 :: (Monad m, P.Proxy p)
 => String                                 -- ^ New default error message
 -> P.Consumer (ParseP a p) (Maybe a) m r  -- ^ Parser to modify
 -> P.Consumer (ParseP a p) (Maybe a) m r
setError str = modifyError (\_ -> str)

-- | Infix version of 'setError'
(<?>)
 :: (Monad m, P.Proxy p)
 => String                                 -- ^ New default error message
 -> P.Consumer (ParseP a p) (Maybe a) m r  -- ^ Parser to modify
 -> P.Consumer (ParseP a p) (Maybe a) m r
(<?>) = setError

infixl 0 <?>

newtype ParseFailure = ParseFailure String deriving (Show, Typeable)

instance Exception ParseFailure

{-| Evaluate a non-backtracking parser, returning the result or failing with
    'Nothing'
-}
evalParseP
 :: (Monad m, P.Proxy p)
 => ParseP i p a' a b' b m r
 -> EitherP SomeException p a' a b' b m r
evalParseP p = EitherP (runCodensityP (runEitherP (
    fmapL (toException . ParseFailure) (evalStateP S.empty (unParseP p)))))

{-| Evaluate a non-backtracking parser \'@K@\'leisli arrow, returning the result
    or failing with 'Nothing'
-}
evalParseK
 :: (Monad m, P.Proxy p)
 => (q -> ParseP i p a' a b' b m r)
 -> (q -> EitherP SomeException p a' a b' b m r)
evalParseK k q = evalParseP (k q)

{- $reexport
    @Control.Applicative@ exports useful combinators for 'Functor',
    'Applicative', and 'Alternative', like 'many', ('<|>'), and 'optional'.

    @Control.Monad@' exports useful combinators for 'Monad' and 'MonadPlus',
    like 'replicateM', 'msum', and 'mfilter'.

    @Control.Monad.Trans.Maybe@ exports run functions for the 'MaybeP' proxy
    transformer.
-}
