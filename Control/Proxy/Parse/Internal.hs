{-| This module exposes internal implementation details that might change in the
    future.  I only expose this so that people can write high-efficiency parsing
    primitives not implementable in terms of existing primitives.
-}
module Control.Proxy.Parse.Internal (
    -- * Parsing proxy transformer
    ParseP(..),

    -- * Utilities
    get,
    put
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Monad.ST (ST)
import qualified Control.Proxy as P
import Control.Proxy ((->>), (>>~), (?>=))
import Data.STRef (STRef, readSTRef, writeSTRef)
import Control.Proxy.Trans.Reader (ReaderP, ask)

{-| Use 'ParseP' to :

    * abstract over end-of-input checks, and

    * push back unused input.
-}
newtype ParseP s i p a' a b' b m r =
    ParseP { unParseP :: ReaderP (STRef s [Maybe i]) p a' a b' b m r }

-- Deriving Functor
instance (P.Proxy p, Monad m) => Functor (ParseP i s p a' a b' b m) where
    fmap f p = ParseP (fmap f (unParseP p))

-- Deriving Applicative
instance (P.Proxy p, Monad m) => Applicative (ParseP i s p a' a b' b m) where
    pure r  = ParseP (pure r)
    f <*> x = ParseP (unParseP f <*> unParseP x)

-- Deriving Monad
instance (P.Proxy p, Monad m) => Monad (ParseP i s p a' a b' b m) where
    return = P.return_P
    (>>=)  = (?>=)

-- Deriving MonadTrans
instance (P.Proxy p) => MonadTrans (ParseP i s p a' a b' b) where
    lift = P.lift_P

-- Deriving MFunctor
instance (P.Proxy p) => P.MFunctor (ParseP i s p a' a b' b) where
    hoist = P.hoist_P

-- Deriving MonadIO
instance (MonadIO m, P.Proxy p) => MonadIO (ParseP i s p a' a b' b m) where
    liftIO = P.liftIO_P

-- Deriving ProxyInternal
instance (P.Proxy p) => P.ProxyInternal (ParseP i s p) where
    return_P = \r -> ParseP (P.return_P r)
    m ?>= f  = ParseP (unParseP m ?>= \r -> unParseP (f r))

    lift_P m = ParseP (P.lift_P m)

    hoist_P nat p = ParseP (P.hoist_P nat (unParseP p))

    liftIO_P m = ParseP (P.liftIO_P m)

-- Deriving Proxy
instance (P.Proxy p) => P.Proxy (ParseP s i p) where
    fb' ->> p = ParseP ((\b' -> unParseP (fb' b')) ->> unParseP p)
    p >>~ fb  = ParseP (unParseP p >>~ (\b -> unParseP (fb b)))

    request = \a' -> ParseP (P.request a')
    respond = \b  -> ParseP (P.respond b )

instance P.ProxyTrans (ParseP s i) where
    liftP p = ParseP (P.liftP p)

instance P.PFunctor (ParseP s i) where
    hoistP nat p = ParseP (P.hoistP nat (unParseP p))

-- | Get the internal leftovers buffer
get :: (P.Proxy p) => ParseP s i p a' a b' b (ST s) [Maybe i]
get = ParseP (do
    ref <- ask
    lift (readSTRef ref) )
{-# INLINABLE get #-}

-- | Set the internal leftovers buffer
put :: (P.Proxy p) => [Maybe i] -> ParseP s i p a' a b' b (ST s) ()
put s = ParseP (do
    ref <- ask
    lift (writeSTRef ref s) )
{-# INLINABLE put #-}
