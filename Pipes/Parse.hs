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
    using,
    fmapPull,
    returnPull,
    bindPull,

    -- * Type Synonyms
    Draw,
    Sink,
    Source,
    Conduit,

    -- * Re-exports
    -- $reexports
    module Control.Monad.Trans.State.Strict
    ) where

import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (
    StateT(runStateT), evalStateT, execStateT, runState, evalState, execState )
import qualified Control.Monad.Trans.State.Strict as S
import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P

{- $pushback
    'unDraw' stores all leftovers in a 'StateP' buffer and 'draw' retrieves
    leftovers in last-in-first-out (LIFO) order from this buffer before drawing
    new input from upstream.
-}

{-| Like @request ()@, except try to use the leftovers buffer first

    A 'Nothing' return value indicates end of input.
-}
draw :: (Monad m) => Sink a (StateT [a] m) (Maybe a)
draw = do
    s <- lift S.get
    case s of
        []   -> request Draw
        a:as -> do
            lift $ S.put as
            return (Just a)
{-# INLINABLE draw #-}

-- | Push an element back onto the leftovers buffer
unDraw :: (Monad m) => a -> Effect (StateT [a] m) ()
unDraw a = lift $ S.modify (a:)
{-# INLINABLE unDraw #-}

-- | Peek at the next element without consuming it
peek :: (Monad m) => Sink a (StateT [a] m) (Maybe a)
peek = do
    ma <- draw
    case ma of
        Nothing -> return ()
        Just a  -> unDraw a
    return ma
{-# INLINABLE peek #-}

-- | Check if at end of input stream.
isEndOfInput :: (Monad m) => Sink a (StateT [a] m) Bool
isEndOfInput = do
    ma <- peek
    case ma of
        Nothing -> return True
        Just _  -> return False
{-# INLINABLE isEndOfInput #-}

{-| Fold all input into a list

    Note: 'drawAll' is usually an anti-pattern.
-}
drawAll :: (Monad m) => () -> Sink a (StateT [a] m) [a]
drawAll = \() -> go id
  where
    go diffAs = do
        ma <- draw
        case ma of
            Nothing -> return (diffAs [])
            Just a  -> go (diffAs . (a:))
{-# INLINABLE drawAll #-}

-- | Consume the input completely, discarding all values
skipAll :: (Monad m) => () -> Sink a (StateT [a] m) ()
skipAll = \() -> go
  where
    go = do
        ma <- draw
        case ma of
            Nothing -> return ()
            Just _  -> go
{-# INLINABLE skipAll #-}

-- | Forward up to the specified number of elements downstream
passUpTo :: (Monad m) => Int -> Draw -> Conduit a a (StateT [a] m) r
passUpTo n0 = \_ -> go n0
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
passWhile :: (Monad m) => (a -> Bool) -> Draw -> Conduit a a (StateT [a] m) r
passWhile pred = \_ -> go
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
wrap
    :: (Monad m)
    => (()   -> Proxy a' a ()          b  m r)
    -> (Draw -> Proxy a' a Draw (Maybe b) m s)
wrap f _ = do
    (f ()) //> \b -> do
        respond (Just b)
        return ()
    forever $ respond Nothing
{-# INLINABLE wrap #-}

{-| Compose 'unwrap' downstream of a guarded pipe to unwrap all 'Just's and
    terminate on the first 'Nothing'.
-}
unwrap :: (Monad m) => () -> Proxy Draw (Maybe a) () a (StateT [a] m) ()
unwrap () = go
  where
    go = do
        ma <- draw
        case ma of
            Nothing -> return ()
            Just a  -> do
                respond a
                go
{-# INLINABLE unwrap #-}

-- | Run a sub-parser with an isolated leftovers buffer
using
    :: (Monad m)
    => s -> Proxy a' a b' b (StateT s m) r -> Proxy a' a b' b (StateT s m) r
using s = hoist lift . evalStateP s

{-| Lift a 'Maybe'-oblivious pipe to a 'Maybe'-aware pipe by auto-forwarding
    all 'Nothing's.

> fmapPull f >-> fmapPull g = fmapPull (f >-> g)
>
> fmapPull pull = pull
-}
fmapPull
    :: (Monad m)
    => (()   -> Pipe    a b m r)
    -> (Draw -> Conduit a b m r)
fmapPull f = bindPull (f >-> returnPull)
{-# INLINABLE fmapPull #-}

-- | Wrap all values flowing downstream in 'Just'.
returnPull :: (Monad m) => Draw -> Proxy () a Draw (Maybe a) m r
returnPull _ = forever $ do
    a <- request ()
    respond (Just a)
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
    => (Draw -> Proxy () a Draw (Maybe b) m r)
    -> (Draw -> Conduit  a             b  m r)
bindPull f d = evalStateP d $ (up \>\ hoist lift . f />/ dn) d
  where
    up () = do
        d  <- lift S.get
        ma <- request d
        case ma of
            Nothing -> do
                d2 <- respond Nothing
                lift $ S.put d
                up ()
            Just a  -> return a
    dn mb = do
        d <- respond mb
        lift $ S.put d
        return d
{-# INLINABLE bindPull #-}

-- | The 'Draw' type forces 
data Draw = Draw

-- | Like a 'Consumer', but can only use 'draw' to request input
type Sink    a   m r = forall y' y . Proxy Draw (Maybe a) y' y m r

-- | Like a 'Producer', except downstream pipes must use 'draw'
type Source    b m r = forall x' x . Proxy x' x Draw (Maybe b) m r

-- | Like a 'Pipe', but may only 'draw' or transmit 'draw' requests
type Conduit a b m r = Proxy Draw (Maybe a) Draw (Maybe b) m r

{- $reexports

    Control.Monad.Trans.State re-exports the 'StateT' type and all the run
    functions.
-}
