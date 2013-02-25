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

    -- * Run functions
    evalParseP,
    evalParseK,

    -- * End of input utilities
    only,
    onlyK
    ) where

import qualified Control.Proxy as P
import Control.Proxy.Parse.Internal (ParseP(ParseP, unParseP), only, onlyK)
import Control.Proxy.Trans.State (StateP(StateP), runStateP, evalStateP)
import Control.Proxy.Trans.Maybe (MaybeP(MaybeP, runMaybeP))
import qualified Data.Sequence as S
import Data.Sequence (ViewL((:<)))

draw :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) String m a
draw = ParseP (StateP (\s -> MaybeP (P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> do
                P.respond "draw: End of input"
                return Nothing
            Just a  -> return (Just (a, s))
    ma:<mas  -> case ma of
        Nothing -> do
            P.respond "draw: End of input"
            return Nothing
        Just a  -> return (Just (a, mas)) ))))

skip :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) String m ()
skip = ParseP (StateP (\s -> MaybeP (P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> do
                P.respond "skip: End of input"
                return Nothing
            Just _  -> return (Just ((), s))
    ma:<mas  -> case ma of
        Nothing -> do
            P.respond "skip: End of input"
            return Nothing
        Just _  -> return (Just ((), mas)) ))))

drawIf
 :: (Monad m, P.Proxy p)
 => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) String m a
drawIf pred = ParseP (StateP (\s -> MaybeP (P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> do
                P.respond "drawIf: End of input"
                return Nothing
            Just a  ->
                if (pred a)
                    then return (Just (a, s))
                    else do
                        P.respond "drawIf: Element failed predicate"
                        return Nothing
    ma:<mas  -> case ma of
        Nothing -> do
            P.respond "drawIf: End of input"
            return Nothing
        Just a  ->
            if (pred a)
                then return (Just (a, mas))
                else do
                    P.respond "drawIf: Element failed predicate"
                    return Nothing ))))

skipIf
 :: (Monad m, P.Proxy p)
 => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) String m ()
skipIf pred = ParseP (StateP (\s -> MaybeP (P.runIdentityP (case S.viewl s of
    S.EmptyL -> do
        ma <- P.request ()
        case ma of
            Nothing -> do
                P.respond "skipIf: End of input"
                return Nothing
            Just a  ->
                if (pred a)
                then return (Just ((), s))
                else do
                    P.respond "skipIf: Element failed predicate"
                    return Nothing
    ma:<mas  -> case ma of
        Nothing -> do
            P.respond "skipIf: End of input"
            return Nothing
        Just a  ->
            if (pred a)
                then return (Just ((), mas))
                else do
                    P.respond "skipIf: Element failed predicate"
                    return Nothing ))))

drawN
 :: (Monad m, P.Proxy p) => Int -> P.Pipe (ParseP a p) (Maybe a) String m [a]
drawN n0 = ParseP (StateP (\s0 -> MaybeP (P.runIdentityP (go0 id s0 n0)))) where
    go0 diffAs s n = if (n > 0)
        then case S.viewl s of
            S.EmptyL -> go1 diffAs n
            ma:<mas  -> case ma of
                Nothing -> err n
                Just a  -> go0 (diffAs . (a:)) mas $! (n - 1)
        else return (Just (diffAs [], s))
    go1 diffAs n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> err n
                Just a  -> go1 (diffAs . (a:)) $! (n - 1)
        else return (Just (diffAs [], S.empty))
    err nLeft = do
        P.respond (
            "drawN " ++ show n0 ++ ": Found only " ++ show (n0 - nLeft)
         ++ " elements" )
        return Nothing
{-# INLINABLE drawN #-}

skipN
 :: (Monad m, P.Proxy p) => Int -> P.Pipe (ParseP a p) (Maybe a) String m ()
skipN n0 = ParseP (StateP (\s0 -> MaybeP (P.runIdentityP (go0 s0 n0)))) where
    go0 s n = if (n > 0)
        then case S.viewl s of
            S.EmptyL -> go1 n
            ma:<mas  -> case ma of
                Nothing -> err n
                Just _  -> go0 mas $! (n - 1)
        else return (Just ((), s))
    go1 n = if (n > 0)
        then do
            ma <- P.request ()
            case ma of
                Nothing -> err n
                Just _  -> go1 $! (n - 1)
        else return (Just ((), S.empty))
    err nLeft = do
        P.respond (
            "skipN " ++ show n0 ++ ": Found only " ++ show (n0 - nLeft)
         ++ " elements" )
        return Nothing
{-# INLINABLE skipN #-}

{-| Evaluate a non-backtracking parser, returning the result or failing with
    'Nothing' -}
evalParseP
 :: (Monad m, P.Proxy p) => ParseP i p a' a b' b m r -> MaybeP p a' a b' b m r
evalParseP p = evalStateP S.empty (unParseP p)

{-| Evaluate a non-backtracking parser \'@K@\'leisli arrow, returning the result
    or failing with 'Nothing' -}
evalParseK
 :: (Monad m, P.Proxy p)
 => (q -> ParseP i p a' a b' b m r) -> (q -> MaybeP p a' a b' b m r)
evalParseK k q = evalParseP (k q)
