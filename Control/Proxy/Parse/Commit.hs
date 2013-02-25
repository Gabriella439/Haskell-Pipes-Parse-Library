{-| This module defines the core machinery for non-backtracking parsers

    You can directly interleave non-backtracking parsing code with other @pipes@
    code, unlike backtracking parsers, which you must first 'commit' before
    embedding within a pipe. -}

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
    protect,
    nextInput,

    -- * Diagnostic messages
    parseDebug,
    parseError,
    silence,
    (<?>),

    -- * Run functions
    evalParseP,
    evalParseK,

    -- * End of input utilities
    only,
    onlyK,

    -- * Re-exports
    -- $reexport
    module Control.Applicative,
    module Control.Monad
    ) where

import Control.Applicative (
    Applicative(pure, (<*>), (<*), (*>)), Alternative(empty, (<|>), some, many))
import Control.Monad (forever)
import qualified Control.Proxy as P
import Control.Proxy ((>>~))
import Control.Proxy.Parse.Internal (ParseP(ParseP, unParseP), only, onlyK)
import Control.Proxy.Trans.Codensity (runCodensityP)
import Control.Proxy.Trans.State (StateP(StateP), evalStateP)
import Control.Proxy.Trans.Maybe (MaybeP(MaybeP, runMaybeP))
import qualified Data.Sequence as S
import Data.Sequence (ViewL((:<)), (<|))

-- For re-exports
import Control.Applicative ((<$>), (<$), (<**>), optional)
import Control.Monad (
    replicateM_, MonadPlus(mzero, mplus), msum, mfilter, guard )

-- | Request a single element
draw :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) String m a
draw = ParseP (MaybeP (StateP (\s -> P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> do
                P.respond "draw: End of input"
                return (Nothing, S.singleton ma)
            Just a  -> return (Just a, s)
    ma:<mas  -> case ma of
        Nothing -> do
            P.respond "draw: End of input"
            return (Nothing, s)
        Just a  -> return (Just a, mas) ))))

-- | Skip a single element
skip :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) String m ()
skip = ParseP (MaybeP (StateP (\s -> P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> do
                P.respond "skip: End of input"
                return (Nothing, S.singleton ma)
            Just _  -> return (Just (), s)
    ma:<mas  -> case ma of
        Nothing -> do
            P.respond "skip: End of input"
            return (Nothing, s)
        Just _  -> return (Just (), mas) ))))

-- | Request a single element that must satisfy the predicate
drawIf
 :: (Monad m, P.Proxy p)
 => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) String m a
drawIf pred = ParseP (MaybeP (StateP (\s -> P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> do
                P.respond "drawIf: End of input"
                return (Nothing, S.singleton ma)
            Just a  ->
                if (pred a)
                    then return (Just a, s)
                    else do
                        P.respond "drawIf: Element failed predicate"
                        return (Nothing, S.singleton ma)
    ma:<mas  -> case ma of
        Nothing -> do
            P.respond "drawIf: End of input"
            return (Nothing, s)
        Just a  ->
            if (pred a)
                then return (Just a, mas)
                else do
                    P.respond "drawIf: Element failed predicate"
                    return (Nothing, s) ))))

-- | Skip a single element that must satisfy the predicate
skipIf
 :: (Monad m, P.Proxy p)
 => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) String m ()
skipIf pred = ParseP (MaybeP (StateP (\s -> P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> do
                P.respond "skipIf: End of input"
                return (Nothing, S.singleton ma)
            Just a  ->
                if (pred a)
                then return (Just (), s)
                else do
                    P.respond "skipIf: Element failed predicate"
                    return (Nothing, S.singleton ma)
    ma:<mas  -> case ma of
        Nothing -> do
            P.respond "skipIf: End of input"
            return (Nothing, s)
        Just a  ->
            if (pred a)
                then return (Just (), mas)
                else do
                    P.respond "skipIf: Element failed predicate"
                    return (Nothing, s) ))))

-- | Request a fixed number of elements
drawN
 :: (Monad m, P.Proxy p) => Int -> P.Pipe (ParseP a p) (Maybe a) String m [a]
drawN n0 = ParseP (MaybeP (StateP (\s0 -> P.runIdentityP (go0 id s0 n0)))) where
    go0 diffAs s n = if (n > 0)
        then case S.viewl s of
            S.EmptyL -> go1 diffAs n
            ma:<mas  -> case ma of
                Nothing -> do
                    err n
                    return (Nothing, s)
                Just a  -> go0 (diffAs . (a:)) mas $! (n - 1)
        else return (Just (diffAs []), s)
    go1 diffAs n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> do
                    err n
                    return (Nothing, S.singleton ma)
                Just a  -> go1 (diffAs . (a:)) $! (n - 1)
        else return (Just (diffAs []), S.empty)
    err nLeft =
        P.respond (
            "drawN " ++ show n0 ++ ": Found only " ++ show (n0 - nLeft)
         ++ " elements" )
{-# INLINABLE drawN #-}

{-| Skip a fixed number of elements

    Faster than 'drawN' if you don't need the input -}
skipN
 :: (Monad m, P.Proxy p) => Int -> P.Pipe (ParseP a p) (Maybe a) String m ()
skipN n0 = ParseP (MaybeP (StateP (\s0 -> P.runIdentityP (go0 s0 n0)))) where
    go0 s n = if (n > 0)
        then case S.viewl s of
            S.EmptyL -> go1 n
            ma:<mas  -> case ma of
                Nothing -> do
                    err n
                    return (Nothing, s)
                Just _  -> go0 mas $! (n - 1)
        else return (Just (), s)
    go1 n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> do
                    err n
                    return (Nothing, S.singleton ma)
                Just _  -> go1 $! (n - 1)
        else return (Just (), S.empty)
    err nLeft =
        P.respond (
            "skipN " ++ show n0 ++ ": Found only " ++ show (n0 - nLeft)
         ++ " elements" )
{-# INLINABLE skipN #-}

-- | Request as many consecutive elements satisfying a predicate as possible
drawWhile
 :: (Monad m, P.Proxy p)
 => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) String m [a]
drawWhile pred = ParseP (MaybeP (StateP (\s0 -> P.runIdentityP (go0 id s0))))
  where
    go0 diffAs s = case S.viewl s of
        S.EmptyL -> go1 diffAs
        ma:<mas  -> case ma of
            Nothing -> return (Just (diffAs []), s)
            Just a  ->
                if (pred a)
                    then go0 (diffAs . (a:)) mas
                    else return (Just (diffAs []), s)
    go1 diffAs = do
        ma <- P.request ()
        case ma of
            Nothing -> return (Just (diffAs []), S.singleton ma)
            Just a  ->
                if (pred a)
                    then go1 (diffAs . (a:))
                    else return (Just (diffAs []), S.singleton ma)
{-# INLINABLE drawWhile #-}

{-| Request as many consecutive elements satisfying a predicate as possible

    Faster than 'drawWhile' if you don't need the input -}
skipWhile
 :: (Monad m, P.Proxy p)
 => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) String m ()
skipWhile pred = ParseP (MaybeP (StateP (\s0 -> P.runIdentityP (go0 s0))))
  where
    go0 s = case S.viewl s of
        S.EmptyL -> go1
        ma:<mas  -> case ma of
            Nothing -> return (Just (), s)
            Just a  ->
                if (pred a)
                    then go0 mas
                    else return (Just (), s)
    go1 = do
        ma <- P.request ()
        case ma of
            Nothing -> return (Just (), S.singleton ma)
            Just a  ->
                if (pred a)
                    then go1
                    else return (Just (), S.singleton ma)
{-# INLINABLE skipWhile #-}

-- | Request the rest of the input
drawAll
 :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) String m [a]
drawAll = ParseP (MaybeP (StateP (\s0 -> P.runIdentityP (go0 id s0)))) where
    go0 diffAs s = case S.viewl s of
        S.EmptyL -> go1 diffAs
        ma:<mas  -> case ma of
            Nothing -> return (Just (diffAs []), s)
            Just a  -> go0 (diffAs . (a:)) mas
    go1 diffAs = do
        ma <- P.request ()
        case ma of
            Nothing -> return (Just (diffAs []), S.singleton ma)
            Just a  -> go1 (diffAs . (a:))
{-# INLINABLE drawAll #-}

{-| Skip the rest of the input

    Faster than 'drawAll' if you don't need the input -}
skipAll
 :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) String m ()
skipAll = ParseP (MaybeP (StateP (\s0 -> P.runIdentityP (go0 s0)))) where
    go0 s = case S.viewl s of
        S.EmptyL -> go1
        ma:<mas  -> case ma of
            Nothing -> return (Just (), s)
            Just a  -> go0 mas
    go1 = do
        ma <- P.request ()
        case ma of
            Nothing -> return (Just (), S.singleton ma)
            Just a  -> go1
{-# INLINABLE skipAll #-}

-- | Push a single element into the leftover buffer
unDraw :: (Monad m, P.Proxy p) => a -> P.Pipe (ParseP a p) (Maybe a) String m ()
unDraw a = ParseP (MaybeP (StateP (\s -> P.runIdentityP (
    return (Just (), Just a <| s) ))))

{-| Look ahead one element without consuming it

    Faster than 'draw' followed by 'unDraw' -}
peek :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) String m a
peek = ParseP (MaybeP (StateP (\s -> P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> do
                P.respond "peek: End of input"
                return (Nothing, S.singleton ma)
            Just a  -> return (Just a, S.singleton ma)
    ma:<mas  -> case ma of
        Nothing -> do
            P.respond "peek: End of input"
            return (Nothing, s)
        Just a  -> return (Just a, s) ))))

-- | Match end of input without consuming it
endOfInput :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) String m ()
endOfInput = ParseP (MaybeP (StateP (\s -> P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> return (Just (), S.singleton ma)
            Just a  -> do
                P.respond "endOfInput: Not end of input"
                return (Nothing, S.singleton ma)
    ma:<mas  -> case ma of
        Nothing -> return (Just (), s)
        Just a  -> do
            P.respond "endOfInput: Not end of input"
            return (Nothing, s) ))))

{- | Protect a parser from failing on end of input by returning 'Nothing'
     instead

> protect p = (Just <$> p) <|> (Nothing <$ endOfInput)
-}
protect
 :: (Monad m, P.Proxy p)
 => P.Pipe (ParseP a p) (Maybe a) String m r
 -> P.Pipe (ParseP a p) (Maybe a) String m (Maybe r)
protect p = fmap Just p <|> fmap (\_ -> Nothing) endOfInput

{-| Consume the end of input token (i.e. 'Nothing'), advancing to the next input

    This is the only primitive that consumes the end of input token.
-}
nextInput :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) String m ()
nextInput = ParseP (MaybeP (StateP (\s -> P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> return (Just (), S.empty)
            Just a  -> do
                P.respond "nextInput: Not end of input"
                return (Nothing, S.singleton ma)
    ma:<mas  -> case ma of
        Nothing -> return (Just (), mas)
        Just a  -> do
            P.respond "nextInput: Not end of input"
            return (Nothing, s) ))))

-- | Emit a diagnostic message and continue parsing
parseDebug
 :: (Monad m, P.Proxy p) => String -> P.Pipe (ParseP a p) (Maybe a) String m ()
parseDebug = P.respond

-- | Emit a diagnostic message and abort parsing
parseError
 :: (Monad m, P.Proxy p) => String -> P.Pipe (ParseP a p) (Maybe a) String m r
parseError str = do
    P.respond str
    mzero

-- | Silence all diagnostic messages emitted from the given parser
silence
 :: (Monad m, P.Proxy p)
 => P.Pipe (ParseP a p) (Maybe a) String m r  -- ^ Parser to silence
 -> P.Pipe (ParseP a p) (Maybe a) String m r
silence p = p >>~ trash where
    trash _ = forever (P.request ())

{-| Mark the entry and exit points of the given parser with a 'String' name

> p <?> "My Parser"

    .. produces the following diagnostic messages:

> Enter: My Parser
> ...
> Leave: My Parser

    ('<?>') only emits the latter message if the parser succeeds.
-}
(<?>)
 :: (Monad m, P.Proxy p)
 => P.Pipe (ParseP a p) (Maybe a) String m r  -- ^ Parser to diagnose
 -> String                                    -- ^ Descriptive label
 -> P.Pipe (ParseP a p) (Maybe a) String m r
p <?> str = do
    parseDebug ("Enter: " ++ str)
    r <- p
    parseDebug ("Leave: " ++ str)
    return r

infixl 0 <?>

{-| Evaluate a non-backtracking parser, returning the result or failing with
    'Nothing' -}
evalParseP
 :: (Monad m, P.Proxy p) => ParseP i p a' a b' b m r -> MaybeP p a' a b' b m r
evalParseP p =
    MaybeP (runCodensityP (evalStateP S.empty (runMaybeP (unParseP p))))

{-| Evaluate a non-backtracking parser \'@K@\'leisli arrow, returning the result
    or failing with 'Nothing' -}
evalParseK
 :: (Monad m, P.Proxy p)
 => (q -> ParseP i p a' a b' b m r) -> (q -> MaybeP p a' a b' b m r)
evalParseK k q = evalParseP (k q)

{- $reexport
    These modules re-export useful combinators for 'Functor', 'Applicative',
    'Monad', 'Alternative', and 'MonadPlus' for use with parsers. -}
