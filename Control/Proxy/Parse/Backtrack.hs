{-| This module defines the core machinery for both backtracking and
    non-backtracking parsers.
-}

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Parse.Backtrack (
    -- * Backtracking parsers
    ParseT(..),

    -- * Single-element parsers
    draw,
    skip,
    drawIf,
    skipIf,

    -- * High-efficiency bulk parsers
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
    protect,
    nextInput,

    -- * Diagnostic messages
    parseDebug,
    parseError,
    silence,
    (<?>),

    -- * Run functions
    runParseT,
    debugParseT,

    -- * End-of-input utilities
    only,
    onlyK,

    -- * Non-backtracking Parsing
{-
    commit,
-}

    -- * Re-exports
    -- $reexport
    module Control.Applicative,
    module Control.Monad,
    S.replicateA,

    -- * Generic combinators
    few,
    anything,
    ) where

import Control.Applicative (
    Applicative(pure, (<*>), (<*), (*>)), Alternative(empty, (<|>), some, many))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State.Strict (StateT(StateT, runStateT))
import Control.Monad.Trans.Error (ErrorT(ErrorT, runErrorT))
import qualified Control.Proxy as P
import Control.Proxy ((>>~), (//>))
import Control.Proxy.Trans.Maybe (MaybeP, nothing)
import Control.Proxy.Trans.Codensity (CodensityP, runCodensityP)
import Control.Proxy.Trans.State (
    StateP(StateP), runStateP, evalStateP, runStateK, evalStateK )
import Data.Monoid (Monoid(mempty), (<>))
import qualified Data.Sequence as S
import Data.Sequence (ViewL((:<)), (<|))

-- For re-exports
import Control.Applicative ((<$>), (<$), (<**>), optional)
import Control.Monad (replicateM_, msum, mfilter, guard)

{-| Use 'ParseT' to:

    * parse input incrementally,

    * backtrack unlimitedly on failure,

    * return all parsing solutions,

    * interleave side effects with parsing, and

    * diagnose parse failures with a stream of informative error messages.
-}
newtype ParseT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a m r =
    ParseT { unParseT ::
        StateT (S.Seq (Maybe a)) (
            ErrorT String (
                P.RespondT (CodensityP p) () (Maybe a) (S.Seq (Maybe a))
                    m ) ) r }
{- To understand the ParseT type, begin from a Hutton-Meijer parser:

> StateT leftovers [] r

   Now replace the list monad with "ListT" (i.e. ProduceT) so that you now have
   a monad transformer:

> StateT leftovers (ProduceT m) r

   Now generalize 'ProduceT' to 'RespondT', for two reasons:

   * Allow requests for more 'input'

   * Return 'drawn' input so that backtracking can reuse it

> StateT leftovers (RespondT p () input drawn m) r

   Now add 'ErrorT' for error messages:

> StateT leftovers (ErrorT String (RespondT p () input drawn m)) r

   Finally, layer Codensity over the base proxy so that you don't pay a
   quadratic time complexity for backtracking:

> StateT leftovers (ErrorT String (RespondT (Codensity p) () input drawn m)) r
-}

-- Deriving Functor
instance (Monad m, P.Interact p) => Functor (ParseT p a m) where
    fmap f p = ParseT (fmap f (unParseT p))

-- Deriving Applicative
instance (Monad m, P.Interact p) => Applicative (ParseT p a m) where
    pure r  = ParseT (pure r)
    f <*> x = ParseT (unParseT f <*> unParseT x)

-- Deriving Monad
instance (Monad m, P.Interact p) => Monad (ParseT p a m) where
    return r = ParseT (return r)
    m >>= f  = ParseT (unParseT m >>= \r -> unParseT (f r))

instance (P.Interact p) => MonadTrans (ParseT p i) where
    lift m = ParseT (lift (lift (lift m)))

-- NOT deriving Alternative
instance (Monad m, P.Interact p) => Alternative (ParseT p a m) where
    empty = ParseT (StateT (\_ -> ErrorT (P.RespondT (return mempty))))
    p1 <|> p2 = ParseT (StateT (\s -> ErrorT (P.RespondT (do
        draw1 <- P.runRespondT (runErrorT (
            runStateT (unParseT p1)  s           ))
        draw2 <- P.runRespondT (runErrorT (
            runStateT (unParseT p2) (s <> draw1) ))
        return (draw1 <> draw2) ))))
{- Backtracking reuses drawn input from the first branch so that the second
   branch begins from the correct leftover state.  If you omit all the newtypes,
   the above methods are equivalent to:

   empty = \_ -> return mempty

   p1 <|> p2 = \s -> do
       draw1 <- p1  s
       draw2 <- p2 (s <> draw)
       return (draw1 <> draw2)

   ... and it's simple to show that these satisfy the Alternative laws:

   p <|> empty = p
   empty <|> p = p
   (p1 <|> p2) <|> p3 = p1 <|> (p2 <|> p3)
-}

instance (Monad m, P.Interact p) => MonadPlus (ParseT p a m) where
    mzero = empty
    mplus = (<|>)

{- NOTE: Although I define ParseT in terms of a monad transformer stack, I
   completely bypass using the stack and inline the logic for the parsing
   primitives, for two reasons:

   * This is the only way to access 'request' beneath 'RespondT', since I don't
     expose a high-level API for this

   * It yields much higher-efficiency code

   You could actually inline more by inlining the CodensityP behavior, but I
   found this optimization to be really unpredictable and gave very small gains
   in performance (< 10%).  I chose to give up the small performance boost
   rather than chase a really elusive compiler optimization with a significant
   increase in code complexity.  Besides, if you really want to push the speed
   limit of parsing you should be using the non-backtracking parser anyway.
-}

-- | Request a single element
draw :: (Monad m, P.Interact p) => ParseT p a m a
draw = ParseT (StateT (\s -> ErrorT (P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Left "draw: End of input"
                Just a  -> Right (a, s) ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Left "draw: End of input"
            Just a  -> Right (a, mas) ) ))))

-- | Skip a single element
skip :: (Monad m, P.Interact p) => ParseT p a m ()
skip = ParseT (StateT (\s -> ErrorT (P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Left "skip: End of input"
                Just _  -> Right ((), s) ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Left "skip: End of input"
            Just _  -> Right ((), mas) ) ))))

-- | Request a single element satisfying a predicate
drawIf :: (Monad m, P.Interact p) => (a -> Bool) -> ParseT p a m a
drawIf pred = ParseT (StateT (\s -> ErrorT (P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Left "drawIf: End of input"
                Just a  ->
                    if (pred a)
                        then Right (a, S.empty)
                        else Left "drawIf: Failed predicate" ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Left "drawIf: End of input"
            Just a  ->
                if (pred a)
                    then Right (a, mas)
                    else Left "drawIf: Failed predicate" ) ))))

-- | Skip a single element satisfying a predicate
skipIf :: (Monad m, P.Interact p) => (a -> Bool) -> ParseT p a m ()
skipIf pred = ParseT (StateT (\s -> ErrorT (P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Left "skipIf: End of input"
                Just a  ->
                    if (pred a)
                        then Right ((), S.empty)
                        else Left "skipIf: Failed predicate" ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Left "skipIf: End of input"
            Just a  ->
                if (pred a)
                    then Right ((), mas)
                    else Left "skipIf: Failed predicate" ) ))))

-- | Request a fixed number of elements
drawN :: (Monad m, P.Interact p) => Int -> ParseT p a m [a]
drawN n0 = ParseT (StateT (\s0 -> ErrorT (P.RespondT (go0 id s0 n0))))
  where
    go0 diffAs s n = if (n > 0)
        then case S.viewl s of
            S.EmptyL -> go1 diffAs n
            ma:<mas  -> case ma of
                Nothing -> err n
                Just a  -> go0 (diffAs . (a:)) mas $! (n - 1)
        else P.respond (Right (diffAs [], s))
    go1 diffAs n = if (n > 0)
        then do
            ma <- P.request ()
            fmap (ma <|) (case ma of
                Nothing -> err n
                Just a  -> go1 (diffAs . (a :)) $! (n - 1) )
        else P.respond (Right (diffAs [], S.empty))
    err nLeft = P.respond (Left (
        "drawN " ++ show n0 ++ ": Found " ++ show (n0 - nLeft) ++ " elements" ))
{-# INLINABLE drawN #-}

{-| Skip a fixed number of elements

    Faster than 'drawN' if you don't need the input -}
skipN :: (Monad m, P.Interact p) => Int -> ParseT p a m ()
skipN n0 = ParseT (StateT (\s0 -> ErrorT (P.RespondT (go0 s0 n0))))
  where
    go0 s n = if (n > 0)
        then case S.viewl s of
            S.EmptyL -> go1 n
            ma:<mas  -> case ma of
                Nothing -> err n
                Just _  -> go0 mas $! (n - 1)
        else P.respond (Right ((), s))
    go1 n = if (n > 0)
        then do
            ma <- P.request ()
            fmap (ma <|) (case ma of
                Nothing -> err n
                Just _  -> go1 $! (n - 1) )
        else P.respond (Right ((), S.empty))
    err nLeft = P.respond (Left (
        "skipN " ++ show n0 ++ ": Found " ++ show (n0 - nLeft) ++ " elements"))
{-# INLINABLE skipN #-}

-- | Request as many consecutive elements satisfying a predicate as possible
drawWhile :: (Monad m, P.Interact p) => (a -> Bool) -> ParseT p a m [a]
drawWhile pred = ParseT (StateT (\s0 -> ErrorT (P.RespondT (go0 id s0))))
  where
    go0 diffAs s = case S.viewl s of
        S.EmptyL -> go1 diffAs
        ma:<mas  -> case ma of
            Nothing -> P.respond (Right (diffAs [], s))
            Just a  ->
                if (pred a)
                    then go0 (diffAs . (a:)) mas
                    else P.respond (Right (diffAs [], s))
    go1 diffAs = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond (Right (diffAs [], S.singleton ma))
            Just a  ->
                if (pred a)
                    then go1 (diffAs . (a:))
                    else P.respond (Right (diffAs [], S.singleton ma)) )
{-# INLINABLE drawWhile #-}

{-| Skip as many consecutive elements satisfying a predicate as possible

    Faster than 'drawWhile' if you don't need the input -}
skipWhile :: (Monad m, P.Interact p) => (a -> Bool) -> ParseT p a m ()
skipWhile pred = ParseT (StateT (\s0 -> ErrorT (P.RespondT (go0 s0))))
  where
    go0 s = case S.viewl s of
        S.EmptyL -> go1
        ma:<mas  -> case ma of
            Nothing -> P.respond (Right ((), s))
            Just a  ->
                if (pred a)
                    then go0 mas
                    else P.respond (Right ((), s))
    go1 = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond (Right ((), S.singleton ma))
            Just a  ->
                if (pred a)
                    then go1
                    else P.respond (Right ((), S.singleton ma)) )
{-# INLINABLE skipWhile #-}

-- | Request the rest of the input
drawAll :: (Monad m, P.Interact p) => ParseT p a m [a]
drawAll = ParseT (StateT (\s0 -> ErrorT (P.RespondT (go0 id s0))))
  where
    go0 diffAs s = case S.viewl s of
        S.EmptyL -> go1 diffAs
        ma:<mas  -> case ma of
            Nothing -> P.respond (Right (diffAs [], s))
            Just a  -> go0 (diffAs . (a:)) mas
    go1 diffAs = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond (Right (diffAs [], S.singleton ma))
            Just a  -> go1 (diffAs . (a:)) )
{-# INLINABLE drawAll #-}

{-| Skip the rest of the input

    Faster than 'drawAll' if you don't need the input -}
skipAll :: (Monad m, P.Interact p) => ParseT p a m ()
skipAll = ParseT (StateT (\s0 -> ErrorT (P.RespondT (go0 s0))))
  where
    go0 s = case S.viewl s of
        S.EmptyL -> go1
        ma:<mas  -> case ma of
            Nothing -> P.respond (Right ((), s))
            Just _  -> go0 mas
    go1 = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond (Right ((), S.singleton ma))
            Just _  -> go1 )
{-# INLINABLE skipAll #-}

-- | Push back a single element into the leftover buffer
unDraw :: (Monad m, P.Interact p) => a -> ParseT p a m ()
unDraw a = ParseT (StateT (\s -> ErrorT (P.RespondT (
    P.respond (Right ((), Just a <| s)) ))))

{-| Look ahead one element without consuming it

    Faster than 'draw' followed by 'unDraw' -}
peek :: (Monad m, P.Interact p) => ParseT p a m a
peek = ParseT (StateT (\s -> ErrorT (P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Left "peek: End of input"
                Just a  -> Right (a, S.singleton ma) ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Left "peek: End of input"
            Just a  -> Right (a, s) ) ))))

-- | Match end of input without consuming it
endOfInput :: (Monad m, P.Interact p) => ParseT p a m ()
endOfInput = ParseT (StateT (\s -> ErrorT (P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Right ((), S.singleton ma)
                Just a  -> Left "endOfInput: Not end of input" ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Right ((), s)
            Just a  -> Left "endOfInput: Not end of input" ) ))))

{-| Protect a parser from failing on end of input by returning 'Nothing' instead

> protect p = (Just <$> p) <|> (Nothing <$ endOfInput)
-}
protect :: (Monad m, P.Interact p) => ParseT p a m r -> ParseT p a m (Maybe r)
protect p = (fmap Just p) <|> (fmap (\_ -> Nothing) endOfInput)

{-| Consume the end of input token, advancing to the next input

    This is the only primitive that consumes the end of input token.
-}
nextInput :: (Monad m, P.Interact p) => ParseT p a m ()
nextInput = ParseT (StateT (\s -> ErrorT (P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Right ((), S.empty)
                Just a  -> Left "nextInput: Not end of input" ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Right ((), mas)
            Just a  -> Left "nextInput: Not end of input" ) ))))

-- | Emit a diagnostic message and continue parsing
parseDebug :: (Monad m, P.Interact p) => String -> ParseT p a m ()
parseDebug str = parseError str <|> pure ()

-- | Silence all messages emitted from the given parser
silence :: (Monad m, P.Interact p) => ParseT p a m r -> ParseT p a m r
silence p = ParseT (StateT (\s -> ErrorT (P.RespondT (
    P.runRespondT (runErrorT (runStateT (unParseT p) s)) //> noLefts ))))
  where
    noLefts e = case e of
        Left  _  -> return S.empty
        Right rs -> P.respond (Right rs)

{-| Mark the entry and exit points of the given parser with a 'String' name

> p <?> "My Parser"

    ... produces the following diagnostic messages:

> Enter: My Parser
> ...
> Leave: My Parser

    ('<?>') only emits the latter message if the parser succeeds
-}
(<?>) :: (Monad m, P.Interact p) => ParseT p a m r -> String -> ParseT p a m r
p <?> str = do
    parseDebug ("Enter: " ++ str)
    r <- p
    parseDebug ("Leave: " ++ str)
    return r

{-| Emit a diagnostic message and abort parsing

    Equivalent to @(parseDebug str >> empty)@, but faster -}
parseError :: (Monad m, P.Interact p) => String -> ParseT p a m r
parseError str = ParseT (StateT (\_ -> ErrorT (return (Left str))))

{-| Convert a backtracking parser to a 'Pipe' that incrementally consumes input
    and streams valid parse results -}
runParseT
 :: (Monad m, P.Interact p)
 => ParseT p a m r -> () -> P.Pipe p (Maybe a) r m ()
runParseT p () = runCodensityP (do
    P.runRespondT (runErrorT (runStateT (unParseT p) mempty)) //>
        \x -> do
            case x of
                Left   _     -> return mempty
                Right (r, _) -> do
                    P.respond r
                    return mempty
    return () )

{-| Convert a backtracking parser to a 'Pipe' that incrementally consumes input
    and streams both valid parse results and diagnostic messages -}
debugParseT
 :: (Monad m, P.Interact p)
 => ParseT p a m r -> () -> P.Pipe p (Maybe a) (Either String r) m ()
debugParseT p () = runCodensityP (do
    P.runRespondT (runErrorT (runStateT (unParseT p) mempty)) //>
        \x -> do
            P.respond (case x of
                Left   e     -> Left  e
                Right (r, _) -> Right r )
            return mempty
    return () )

-- | Wrap a proxy's output in 'Just' and finish with a 'Nothing'
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

{-| Wrap a proxy \'@K@\'leisli arrow's output in 'Just' and finish with a
    'Nothing' -}
onlyK
 :: (Monad m, P.Proxy p)
 => (q -> p a' a b' b m r) -> (q -> p a' a b' (Maybe b) m r)
onlyK k q = only (k q)

{-
-- | Convert a backtracking parser to a non-backtracking parser
commit
 :: (Monad m, P.Interact p)
 => ParseT p a m r -> () -> P.Pipe (StateP s (MaybeP p)) (Maybe a) String m r
commit p () = StateP $ \s ->
    (do P.liftP (runCodensityP (P.runRespondT (runErrorT (runStateT (unParseT p) s)) //> \x -> do
            P.respond x
            return mempty ) )
        nothing ) >>~ firstSuccess
  where
    firstSuccess a = do
        case a of
            Left  b -> do
                b' <- P.respond b
                a2 <- P.request b'
                firstSuccess a
            Right rs -> return rs
-}

{- $reexport
    These re-exports provide many useful generic functions that work for
    'ParseT', particularly the 'Alternative' and 'MonadPlus' functions like
    ('<|>'), 'many', and 'msum'.
-}

-- | Like 'many', but orders results from fewest to most matches
few :: (Alternative f) => f a -> f [a]
few fa = go where
    go = pure [] <|> (fmap (:) fa <*> go)

-- | Like 'some', but orders results from fewest to most matches
anything :: (Alternative f) => f a -> f [a]
anything fa = ((:) <$> fa <*> go) where
    go = pure [] <|> (fmap (:) fa <*> go)
