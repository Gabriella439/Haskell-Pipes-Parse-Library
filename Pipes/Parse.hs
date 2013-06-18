{-# LANGUAGE RankNTypes #-}

-- | Parsing utilities for pipes

module Pipes.Parse (
    -- * Pushback and Leftovers
    -- $pushback
    draw,
    unDraw,

    -- * Utilities
    peek,
    isEndOfInput,
    drawAll,
    skipAll,
    passUpTo,
    passWhile,

    -- * Adapters
    -- $adapters
    wrap,
    unwrap,
    fmapPull,
    returnPull,
    bindPull,

    -- Re-exports
    -- $reexports
    module Control.Monad.Trans.State.Strict
    ) where

import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (
    StateT(runStateT), evalStateT, execStateT, runState, evalState, execState )
import qualified Control.Monad.Trans.State.Strict as S
import Pipes
import qualified Pipes.Prelude as P

{- $pushback
    'unDraw' stores all leftovers in a 'StateP' buffer and 'draw' retrieves
    leftovers from this buffer before drawing new input from upstream.
-}

{-| Like @request ()@, except try to use the leftovers buffer first

    A 'Nothing' return value indicates end of input.
-}
draw :: (Monad m) => Consumer (Maybe a) (StateT [a] m) (Maybe a)
draw = do
    s <- lift S.get
    case s of
        []   -> request ()
        a:as -> do
            lift $ S.put as
            return (Just a)
{-# INLINABLE draw #-}

-- | Push an element back onto the leftovers buffer
unDraw :: (Monad m) => a -> Effect (StateT [a] m) ()
unDraw a = lift $ S.modify (a:)
{-# INLINABLE unDraw #-}

-- | Peek at the next element without consuming it
peek :: (Monad m) => Consumer (Maybe a) (StateT [a] m) (Maybe a)
peek = do
    ma <- draw
    case ma of
        Nothing -> return ()
        Just a  -> unDraw a
    return ma
{-# INLINABLE peek #-}

-- | Check if at end of input stream.
isEndOfInput :: (Monad m) => Consumer (Maybe a) (StateT [a] m) Bool
isEndOfInput = do
    ma <- peek
    case ma of
        Nothing -> return True
        Just _  -> return False
{-# INLINABLE isEndOfInput #-}

{-| Fold all input into a list

    Note: 'drawAll' is usually an anti-pattern.
-}
drawAll :: (Monad m) => () -> Consumer (Maybe a) (StateT [a] m) [a]
drawAll = \() -> go id
  where
    go diffAs = do
        ma <- draw
        case ma of
            Nothing -> return (diffAs [])
            Just a  -> go (diffAs . (a:))
{-# INLINABLE drawAll #-}

-- | Consume the input completely, discarding all values
skipAll :: (Monad m) => () -> Consumer (Maybe a) (StateT [a] m) ()
skipAll = \() -> go
  where
    go = do
        ma <- draw
        case ma of
            Nothing -> return ()
            Just _  -> go
{-# INLINABLE skipAll #-}

-- | Forward up to the specified number of elements downstream
passUpTo :: (Monad m) => Int -> () -> Pipe (Maybe a) (Maybe a) (StateT [a] m) r
passUpTo n0 = \() -> go n0
  where
    go n0 =
        if (n0 <= 0)
        then forever $ respond Nothing
        else do
            ma <- draw
            respond ma
            case ma of
                Nothing -> forever $ respond Nothing
                Just _  -> go (n0 - 1)
{-# INLINABLE passUpTo #-}

{-| Forward downstream as many consecutive elements satisfying a predicate as
    possible
-}
passWhile
    :: (Monad m)
    => (a -> Bool) -> () -> Pipe (Maybe a) (Maybe a) (StateT [a] m) r
passWhile pred = \() -> go
  where
    go = do
        ma <- draw
        case ma of
            Nothing -> forever $ respond Nothing
            Just a  ->
                if (pred a)
                then do
                    respond ma
                    go
                else do
                    unDraw a
                    forever $ respond Nothing
{-# INLINABLE passWhile #-}

{- $adapters
    Use 'wrap' and 'unwrap' to convert between guarded and unguarded pipes.

    'fmapPull', 'returnPull', and 'bindPull' promote compatibility with
    existing utilities that are not 'Maybe'-aware.
-}

{-| Guard a pipe from terminating by wrapping every output in 'Just' and ending
    with a never-ending stream of 'Nothing's.
-}
wrap :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' (Maybe b) m s
wrap = \p -> do
    p //> \b -> respond (Just b)
    forever $ respond Nothing
{-# INLINABLE wrap #-}

{-| Compose 'unwrap' downstream of a guarded pipe to unwrap all 'Just's and
    terminate on the first 'Nothing'.
-}
unwrap :: (Monad m) => () -> Pipe (Maybe a) a m ()
unwrap () = go
  where
    go = do
        ma <- request ()
        case ma of
            Nothing -> return ()
            Just a  -> do
                respond a
                go
{-# INLINABLE unwrap #-}

{-| Lift a 'Maybe'-oblivious pipe to a 'Maybe'-aware pipe by auto-forwarding
    all 'Nothing's.

> fmapPull f >-> fmapPull g = fmapPull (f >-> g)
>
> fmapPull pull = pull
-}
fmapPull
    :: (Monad m)
    => (x -> Proxy x        a  x        b  m r)
    -> (x -> Proxy x (Maybe a) x (Maybe b) m r)
fmapPull f = bindPull (f >-> returnPull)
{-# INLINABLE fmapPull #-}

-- | Wrap all values flowing downstream in 'Just'.
returnPull :: (Monad m) => x -> Proxy x a x (Maybe a) m r
returnPull = P.generalize (P.map Just)
{-# INLINABLE returnPull #-}

{-| Lift a 'Maybe'-generating pipe to a 'Maybe'-transforming pipe by
    auto-forwarding all 'Nothing's

> -- Using: f >>> g = f >-> bindPull g
>
> returnPull >>> f = f
>
> f >>> returnPull = f
>
> (f >>> g) >>> h = f >>> (g >>> h)

Or equivalently:

> returnPull >-> bindPull f = f
>
> bindPull returnPull = pull
>
> bindPull (f >-> bindPull g) = bindPull f >-> bindPull g
-}
bindPull
    :: (Monad m)
    => (x -> Proxy x        a  x (Maybe b) m r)
    -> (x -> Proxy x (Maybe a) x (Maybe b) m r)
bindPull f = up \>\ f
  where
    up a' = do
        ma <- request a'
        case ma of
            Nothing -> do
                a'2 <- respond Nothing
                up a'2
            Just a  -> return a
{-# INLINABLE bindPull #-}

{- $reexports

    Control.Monad.Trans.State re-exports the 'StateT' type and all the run
    functions.
-}
