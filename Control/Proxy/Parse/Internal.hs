{-| This module exposes internal implementation details that might change in the
    future.  I only expose this so that people can write high-efficiency parsing
    primitives not implementable in terms of existing primitives.
-}
module Control.Proxy.Parse.Internal (
    -- * Parsing monad transformer
    BufferT(..),
    runBufferT,

    -- * Utilities
    get,
    put
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (liftM, ap)
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class(MonadTrans(lift))
import qualified Control.Monad.Trans.State as S

-- | 'BufferT' stores leftovers.
newtype BufferT i m r = BufferT { unBufferT :: S.StateT [Maybe i] m r }

instance (Monad m) => Functor (BufferT i m) where
    fmap f p = BufferT (liftM f (unBufferT p))

instance (Monad m) => Applicative (BufferT i m) where
    pure r  = BufferT (return r)
    f <*> x = BufferT (ap (unBufferT f) (unBufferT x))

-- Deriving Monad
instance (Monad m) => Monad (BufferT i m) where
    return r = BufferT (return r)
    m >>= f  = BufferT (unBufferT m >>= \r -> unBufferT (f r))

-- Deriving MonadTrans
instance MonadTrans (BufferT i) where
    lift m = BufferT (lift m)

-- Deriving MFunctor
instance MFunctor (BufferT i) where
    hoist nat m = BufferT (hoist nat (unBufferT m))

-- Deriving MonadIO
instance (MonadIO m) => MonadIO (BufferT i m) where
    liftIO m = BufferT (liftIO m)

-- | Unwrap 'BufferT'
runBufferT :: (Monad m) => BufferT i m r -> m r
runBufferT p = S.evalStateT (unBufferT p) []

{-# INLINABLE runBufferT #-}
-- | Get the internal leftovers buffer
get :: (Monad m) => BufferT i m [Maybe i]
get = BufferT S.get
{-# INLINABLE get #-}

-- | Set the internal leftovers buffer
put :: (Monad m) => [Maybe i] -> BufferT i m ()
put s = BufferT (S.put s)
{-# INLINABLE put #-}
