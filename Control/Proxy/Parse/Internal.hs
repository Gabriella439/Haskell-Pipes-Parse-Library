{-| This module exposes internal implementation details that might change in the
    future.  I only expose this so that people can write high-efficiency parsing
    primitives not implementable in terms of existing primitives.
-}
module Control.Proxy.Parse.Internal (
    -- * Parsing proxy transformer
    ParseP(..),

    -- * Utilities
    get,
    put,
    throw,
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Exception (SomeException, Exception, toException)
import qualified Control.Proxy as P
import Control.Proxy ((->>), (>>~), (?>=))
import qualified Control.Proxy.Trans.Either as E
import qualified Control.Proxy.Trans.State as S

{-| Use 'ParseP' for non-backtracking parsing if you want to:

    * stream input in as little memory as possible,

    * diagnose parse failures with error messages,

    * parse input lazily and incrementally, and

    * interleave side effects with parsing.
-}
newtype ParseP i p a' a b' b m r =
    ParseP { unParseP ::
        S.StateP [Maybe i] (E.EitherP SomeException p) a' a b' b m r }

-- Deriving Functor
instance (P.Proxy p, Monad m) => Functor (ParseP i p a' a b' b m) where
    fmap f p = ParseP (fmap f (unParseP p))

-- Deriving Applicative
instance (P.Proxy p, Monad m) => Applicative (ParseP i p a' a b' b m) where
    pure r  = ParseP (pure r)
    f <*> x = ParseP (unParseP f <*> unParseP x)

-- Deriving Monad
instance (P.Proxy p, Monad m) => Monad (ParseP i p a' a b' b m) where
    return = P.return_P
    (>>=)  = (?>=)

-- Deriving MonadTrans
instance (P.Proxy p) => MonadTrans (ParseP i p a' a b' b) where
    lift = P.lift_P

-- Deriving MFunctor
instance (P.Proxy p) => P.MFunctor (ParseP i p a' a b' b) where
    hoist = P.hoist_P

-- Deriving MonadIO
instance (MonadIO m, P.Proxy p) => MonadIO (ParseP i p a' a b' b m) where
    liftIO = P.liftIO_P

-- Deriving ProxyInternal
instance (P.Proxy p) => P.ProxyInternal (ParseP i p) where
    return_P = \r -> ParseP (P.return_P r)
    m ?>= f  = ParseP (unParseP m ?>= \r -> unParseP (f r))

    lift_P m = ParseP (P.lift_P m)

    hoist_P nat p = ParseP (P.hoist_P nat (unParseP p))

    liftIO_P m = ParseP (P.liftIO_P m)

-- Deriving Proxy
instance (P.Proxy p) => P.Proxy (ParseP i p) where
    fb' ->> p = ParseP ((\b' -> unParseP (fb' b')) ->> unParseP p)
    p >>~ fb  = ParseP (unParseP p >>~ (\b -> unParseP (fb b)))

    request = \a' -> ParseP (P.request a')
    respond = \b  -> ParseP (P.respond b )

instance P.ProxyTrans (ParseP i) where
    liftP p = ParseP (P.liftP (P.liftP p))

instance P.PFunctor (ParseP i) where
    hoistP nat p = ParseP (P.hoistP (P.hoistP nat) (unParseP p))

-- | Get the internal leftovers buffer
get :: (Monad m, P.Proxy p) => ParseP i p a' a b' b m [Maybe i]
get = ParseP S.get
{-# INLINABLE get #-}

-- | Set the internal leftovers buffer
put :: (Monad m, P.Proxy p) => [Maybe i] -> ParseP i p a' a b' b m ()
put s = ParseP (S.put s)
{-# INLINABLE put #-}

-- | Fail parsing by throwing an 'Exception'
throw :: (Exception e, Monad m, P.Proxy p) => e -> ParseP i p a' a b' b m r
throw e = ParseP (P.liftP (E.throw (toException e)))
{-# INLINABLE throw #-}
