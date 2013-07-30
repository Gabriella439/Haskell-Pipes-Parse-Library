-- | Parsing utilities for pipes

module Pipes.Parse (
    -- * Primitives
    draw,
    unDraw,

    -- * Producer
    input,

    -- * Isomorphisms
    -- $isomorphisms
    spans,
    splits,

    -- * Re-exports
    -- $re-exports
    module Control.Lens,
    module Control.Monad.IO.Class,
    module Control.Monad.Trans.State
    ) where

import Control.Lens (Iso', zoom)
import qualified Control.Lens as L
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, execStateT)
import qualified Control.Monad.Trans.State as S
import Pipes (Producer, yield, next)
import Pipes.Core (Producer')
import Prelude hiding (splitAt, span)

-- | Draw a single element from the 'Producer'
draw :: (Monad m) => StateT (Producer a m r) m (Maybe a)
draw = do
    p <- S.get
    x <- lift (next p)
    case x of
        Left   _      -> return Nothing
        Right (e, p') -> do
            S.put p'
            return (Just e)

-- | Push a single element back onto the 'Producer'
unDraw :: (Monad m) => a -> StateT (Producer a m r) m ()
unDraw e = S.modify (yield e >>)

{-| Stream from the stored 'Producer'

    Streaming from 'input' differs from streaming directly from the underlying
    'Producer' because any unused input is saved for later.
-}
input :: (Monad m) => Producer' a (StateT (Producer a m r) m) ()
input = loop
  where
    loop = do
        me <- lift draw
        case me of
            Nothing -> return ()
            Just e  -> do
                yield e
                loop

{- $isomorphisms
    Use these isomorphisms with 'zoom' to limit a parser to a subset of the
    input stream:

> printAll :: (Show a) = StateT (Producer a IO r) IO ()
> printAll = run $ for input (liftIO . print)
>
> parser :: StateT (Producer Int IO r) IO ()
> parser = do
>     zoom (spans (< 4)) printAll  -- Limit to consecutive elements below 4
>     liftIO $ putStrLn "Intermission"
>     zoom (splits 3)    printAll  -- Limit to the next 3 elements

>>> evalStateT parser (each [1..])
1
2
3
Intermission
4
5
6

-}

_span
    :: (Monad m)
    => (a -> Bool) -> Producer a m r -> Producer a m (Producer a m r)
_span predicate = loop
  where
    loop p = do
        x <- lift (next p)
        case x of
            Left   r      -> return (return r)
            Right (e, p') ->
                if (predicate e)
                then do
                    yield e
                    loop p'
                else return (yield e >> p')

{-| Isomorphism between a 'Producer' and its 'span'

    Use this isomorphism to limit a 'Producer' to the prefix of all elements
    that satisfy a given predicate.
-}
spans
    :: (Monad m)
    => (a -> Bool) -> Iso' (Producer a m r) (Producer a m (Producer a m r))
spans predicate = L.iso (_span predicate) join

_splitAt :: (Monad m) => Int -> Producer a m r -> Producer a m (Producer a m r)
_splitAt n p =
    if (n <= 0)
    then return p
    else do
        x <- lift $ next p
        case x of
            Left   r      -> return (return r)
            Right (e, p') -> do
                yield e
                _splitAt (n - 1) p'


{-| Isomorphism between a 'Producer' and its 'splitAt'

    Use this isomorphism to limit a 'Producer' to a fixed number of elements.
-}
splits
    :: (Monad m) => Int -> Iso' (Producer a m r) (Producer a m (Producer a m r))
splits n = L.iso (_splitAt n) join

{- $re-exports
    @Control.Lens@ re-exports 'zoom' and 'Iso''.

    @Control.Monad.Trans.State@ re-exports 'StateT' (the type), 'runStateT',
    'evalStateT', and 'execStateT'.

    @Control.Monad.IO.Class@ re-exports 'MonadIO'.
-}
