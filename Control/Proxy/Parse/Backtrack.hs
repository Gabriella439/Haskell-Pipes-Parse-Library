-- | This module defines the core machinery for backtracking parsers.

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Parse.Backtrack (
    -- * Backtracking parsers
    ParseT,

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

    -- * Fail-safe parsers
    drawMay,
    peek,

    -- * End of input
    endOfInput,
    isEndOfInput,
    nextInput,

    -- * Non-backtracking Parsing
    commit,

    -- * Run functions
    evalParseT,

    -- * End-of-input utilities
    only,
    onlyK,
    just,
    justK,

    -- * Re-exports
    -- $reexport
    module Control.Applicative,
    module Control.Monad,

    -- * Generic combinators
    few,
    anything,
    ) where

import Control.Applicative (
    Applicative(pure, (<*>), (<*), (*>)), Alternative(empty, (<|>), some, many))
import Control.Monad.Trans.State.Strict (StateT(StateT, runStateT))
import qualified Control.Proxy as P
import Control.Proxy ((>>~), (//>))
import Control.Proxy.Parse.Internal (
    ParseT(ParseT, unParseT), ParseP(ParseP), only, onlyK, just, justK )
import Control.Proxy.Trans.Codensity (runCodensityP)
import Control.Proxy.Trans.Either (EitherP(EitherP))
import Control.Proxy.Trans.State (StateP(StateP))
import qualified Data.Sequence as S
import Data.Sequence (ViewL((:<)), (<|))

-- For re-exports
import Control.Applicative ((<$>), (<$), (<**>), optional)
import Control.Monad (
    replicateM, replicateM_, MonadPlus(mzero, mplus), msum, mfilter, guard )

{- NOTE: Although I define ParseT in terms of a monad transformer stack, I
   completely bypass using the stack and inline the logic for the parsing
   primitives, for two reasons:

   * This is the only way to access 'request' beneath 'RespondT', since I don't
     expose a high-level API for this

   * It yields much higher-efficiency code

   You could actually inline more by inlining the CodensityP behavior, but I
   found this optimization to be really unpredictable and gave very small gains
   in microbenchmarks (< 10%).  I chose to give up the small performance boost
   rather than chase a really elusive compiler optimization with a significant
   increase in code complexity.  Besides, if you really want to push the speed
   limit of parsing you should be using either 'pipes-attoparsec' or the
   non-backtracking parser.
-}

-- | Request a single element
draw :: (Monad m, P.ListT p) => ParseT p a m a
draw = ParseT (StateT (\s -> P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            case ma of
                Nothing -> return (S.singleton ma)
                Just a  -> fmap (ma <|) (P.respond (a, s))
        ma:<mas  -> case ma of
            Nothing -> return S.empty
            Just a  -> P.respond (a, mas) )))
{-# INLINABLE draw #-}

-- | Skip a single element
skip :: (Monad m, P.ListT p) => ParseT p a m ()
skip = ParseT (StateT (\s -> P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            case ma of
                Nothing -> return (S.singleton ma)
                Just _  -> fmap (ma <|) (P.respond ((), s))
        ma:<mas  -> case ma of
            Nothing -> return S.empty
            Just _  -> P.respond ((), mas) )))
{-# INLINABLE skip #-}

-- | Request a single element that must satisfy the predicate
drawIf :: (Monad m, P.ListT p) => (a -> Bool) -> ParseT p a m a
drawIf pred = ParseT (StateT (\s -> P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            case ma of
                Nothing -> return (S.singleton ma)
                Just a  ->
                    if (pred a)
                        then fmap (ma <|) (P.respond (a, s))
                        else return (S.singleton ma)
        ma:<mas  -> case ma of
            Nothing -> return S.empty
            Just a  ->
                if (pred a)
                    then P.respond (a, mas)
                    else return S.empty )))
{-# INLINABLE drawIf #-}

-- | Skip a single element that must satisfy the predicate
skipIf :: (Monad m, P.ListT p) => (a -> Bool) -> ParseT p a m ()
skipIf pred = ParseT (StateT (\s -> P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            case ma of
                Nothing -> return (S.singleton ma)
                Just a  ->
                    if (pred a)
                        then fmap (ma <|) (P.respond ((), s))
                        else return (S.singleton ma)
        ma:<mas  -> case ma of
            Nothing -> return S.empty
            Just a  ->
                if (pred a)
                    then P.respond ((), mas)
                    else return S.empty )))
{-# INLINABLE skipIf #-}

-- | Request a fixed number of elements
drawN :: (Monad m, P.ListT p) => Int -> ParseT p a m [a]
drawN n0 = ParseT (StateT (\s0 -> P.RespondT (go0 id s0 n0)))
  where
    go0 diffAs s n = if (n > 0)
        then case S.viewl s of
            S.EmptyL -> go1 diffAs n
            ma:<mas  -> case ma of
                Nothing -> return S.empty
                Just a  -> go0 (diffAs . (a:)) mas $! (n - 1)
        else P.respond (diffAs [], s)
    go1 diffAs n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> return (S.singleton ma)
                Just a  -> fmap (ma <|) (go1 (diffAs . (a :)) $! (n - 1))
        else P.respond (diffAs [], S.empty)
{-# INLINABLE drawN #-}

{-| Skip a fixed number of elements

    Faster than 'drawN' if you don't need the input
-}
skipN :: (Monad m, P.ListT p) => Int -> ParseT p a m ()
skipN n0 = ParseT (StateT (\s0 -> P.RespondT (go0 s0 n0)))
  where
    go0 s n = if (n > 0)
        then case S.viewl s of
            S.EmptyL -> go1 n
            ma:<mas  -> case ma of
                Nothing -> return S.empty
                Just _  -> go0 mas $! (n - 1)
        else P.respond ((), s)
    go1 n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> return (S.singleton ma)
                Just _  -> fmap (ma <|) (go1 $! (n - 1))
        else P.respond ((), S.empty)
{-# INLINABLE skipN #-}

-- | Request as many consecutive elements satisfying a predicate as possible
drawWhile :: (Monad m, P.ListT p) => (a -> Bool) -> ParseT p a m [a]
drawWhile pred = ParseT (StateT (\s0 -> P.RespondT (go0 id s0)))
  where
    go0 diffAs s = case S.viewl s of
        S.EmptyL -> go1 diffAs
        ma:<mas  -> case ma of
            Nothing -> P.respond (diffAs [], s)
            Just a  ->
                if (pred a)
                    then go0 (diffAs . (a:)) mas
                    else P.respond (diffAs [], s)
    go1 diffAs = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond (diffAs [], S.singleton ma)
            Just a  ->
                if (pred a)
                    then go1 (diffAs . (a:))
                    else P.respond (diffAs [], S.singleton ma) )
{-# INLINABLE drawWhile #-}

{-| Skip as many consecutive elements satisfying a predicate as possible

    Faster than 'drawWhile' if you don't need the input
-}
skipWhile :: (Monad m, P.ListT p) => (a -> Bool) -> ParseT p a m ()
skipWhile pred = ParseT (StateT (\s0 -> P.RespondT (go0 s0)))
  where
    go0 s = case S.viewl s of
        S.EmptyL -> go1
        ma:<mas  -> case ma of
            Nothing -> P.respond ((), s)
            Just a  ->
                if (pred a)
                    then go0 mas
                    else P.respond ((), s)
    go1 = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond ((), S.singleton ma)
            Just a  ->
                if (pred a)
                    then go1
                    else P.respond ((), S.singleton ma) )
{-# INLINABLE skipWhile #-}

-- | Request the rest of the input
drawAll :: (Monad m, P.ListT p) => ParseT p a m [a]
drawAll = ParseT (StateT (\s0 -> P.RespondT (go0 id s0)))
  where
    go0 diffAs s = case S.viewl s of
        S.EmptyL -> go1 diffAs
        ma:<mas  -> case ma of
            Nothing -> P.respond (diffAs [], s)
            Just a  -> go0 (diffAs . (a:)) mas
    go1 diffAs = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond (diffAs [], S.singleton ma)
            Just a  -> go1 (diffAs . (a:)) )
{-# INLINABLE drawAll #-}

{-| Skip the rest of the input

    Faster than 'drawAll' if you don't need the input
-}
skipAll :: (Monad m, P.ListT p) => ParseT p a m ()
skipAll = ParseT (StateT (\s0 -> P.RespondT (go0 s0)))
  where
    go0 s = case S.viewl s of
        S.EmptyL -> go1
        ma:<mas  -> case ma of
            Nothing -> P.respond ((), s)
            Just _  -> go0 mas
    go1 = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond ((), S.singleton ma)
            Just _  -> go1 )
{-# INLINABLE skipAll #-}

-- | Push back a single element into the leftover buffer
unDraw :: (Monad m, P.ListT p) => a -> ParseT p a m ()
unDraw a = ParseT (StateT (\s -> P.RespondT (
    P.respond ((), Just a <| s) )))
{-# INLINABLE unDraw #-}

{-| Request 'Just' one element or 'Nothing' if at end of input

> drawMay = (Just <$> draw) <|> (Nothing <$ endOfInput)
-}
drawMay :: (Monad m, P.ListT p) => ParseT p a m (Maybe a)
drawMay = ParseT (StateT (\s -> P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (ma, s))
        ma:<mas  -> P.respond (ma, mas) )))
{-# INLINABLE drawMay #-}

{-| Look ahead one element without consuming it

    Faster than 'drawMay' followed by 'unDraw'
-}
peek :: (Monad m, P.ListT p) => ParseT p a m (Maybe a)
peek = ParseT (StateT (\s -> P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (ma, S.singleton ma))
        ma:<mas  -> P.respond (ma, s) )))
{-# INLINABLE peek #-}

-- | Match end of input without consuming it
endOfInput :: (Monad m, P.ListT p) => ParseT p a m ()
endOfInput = ParseT (StateT (\s -> P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            case ma of
                Nothing -> fmap (ma <|) (P.respond ((), S.singleton ma))
                Just a  -> return (S.singleton ma)
        ma:<mas  -> case ma of
            Nothing -> P.respond ((), s)
            Just a  -> return S.empty )))
{-# INLINABLE endOfInput #-}

-- | Return whether cursor is at end of input
isEndOfInput :: (Monad m, P.ListT p) => ParseT p a m Bool
isEndOfInput = ParseT (StateT (\s -> P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> True
                Just _  -> False, S.singleton ma ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> True
            Just _  -> False, mas) )))
{-# INLINABLE isEndOfInput #-}

{-| Consume the end of input token, advancing to the next input

    This is the only primitive that consumes the end of input token.
-}
nextInput :: (Monad m, P.ListT p) => ParseT p a m ()
nextInput = ParseT (StateT (\s -> P.RespondT (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            case ma of
                Nothing -> fmap (ma <|) (P.respond ((), s))
                Just a  -> return (S.singleton ma)
        ma:<mas  -> case ma of
            Nothing -> P.respond ((), mas)
            Just a  -> return S.empty )))
{-# INLINABLE nextInput #-}

{-| Convert a backtracking parser to a non-backtracking parser

    Rewinds to starting point if the backtracking parser fails
-}
commit
 :: (Monad m, P.ListT p)
 => String -> ParseT p a m r -> P.Consumer (ParseP a p) (Maybe a) m r
commit str p = ParseP (StateP (\s -> EitherP (
    (do P.runRespondT (runStateT (unParseT p) s) //> \rs -> do
            P.respond rs
            return S.empty
        return (Left str) ) >>~ \rs -> return (Right rs) )))
{-# INLINABLE commit #-}

{-| Convert a backtracking parser to a 'Pipe' that incrementally consumes input
    and streams valid parse results
-}
evalParseT
 :: (Monad m, P.ListT p) => ParseT p a m r -> () -> P.Pipe p (Maybe a) r m ()
evalParseT p () = runCodensityP (do
    P.runRespondT (runStateT (unParseT p) S.empty) //> \(r, _) -> do
        P.respond r
        return S.empty
    return () )

{- $reexport
    @Control.Applicative@ exports useful combinators for 'Functor',
    'Applicative', and 'Alternative', like 'many', ('<|>'), and 'optional'.

    @Control.Monad@ exports useful combinators for 'Monad' and 'MonadPlus',
    like 'replicateM', 'msum', and 'mfilter'.
-}

-- | Like 'many', but orders results from fewest to most matches
few :: (Alternative f) => f a -> f [a]
few fa = go where
    go = pure [] <|> (fmap (:) fa <*> go)

-- | Like 'some', but orders results from fewest to most matches
anything :: (Alternative f) => f a -> f [a]
anything fa = ((:) <$> fa <*> go) where
    go = pure [] <|> (fmap (:) fa <*> go)
