{-| This module exposes internal implementation details that might change in the
    future.  I only expose this so that people can write high-efficiency parsing
    primitives not implementable in terms of existing primitives.  My own
    benchmarks show that you almost always get equally fast performance using
    'drawMay', sometimes even faster, so you probably never need this module.
-}

{-# LANGUAGE RankNTypes #-}

module Control.Proxy.Parse.Internal (
    -- * Parsing proxy transformer
    ParseP(..),
    runParseP,
    runParseK,

    -- * Utilities
    get,
    put
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import qualified Control.Proxy as P
import Control.Proxy ((->>), (>>~), (>\\), (//>), (?>=))
import qualified Control.Proxy.Trans.State as S

-- | The 'ParseP' proxy transformer stores parsing leftovers
newtype ParseP i p a' a b' b m r =
    ParseP { unParseP :: S.StateP [Maybe i] p a' a b' b m r }

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

    thread_P p = \s -> ParseP (P.thread_P (unParseP p) s)


-- Deriving Proxy
instance (P.Proxy p) => P.Proxy (ParseP i p) where
    fb' ->> p = ParseP ((\b' -> unParseP (fb' b')) ->> unParseP p)
    p >>~ fb  = ParseP (unParseP p >>~ (\b -> unParseP (fb b)))

    request = \a' -> ParseP (P.request a')
    respond = \b  -> ParseP (P.respond b )

    fb' >\\ p = ParseP ((\b' -> unParseP (fb' b')) >\\ unParseP p)
    p //> fb  = ParseP (unParseP p //> (\b -> unParseP (fb b)))

    turn p = ParseP (P.turn (unParseP p))


instance P.ProxyTrans (ParseP i) where
    liftP p = ParseP (P.liftP p)

instance P.PFunctor (ParseP i) where
    hoistP nat p = ParseP (P.hoistP nat (unParseP p))

{-| Evaluate a non-backtracking parser, returning the result or failing with a
    'ParseFailure' exception.
 -}
runParseP :: (Monad m, P.Proxy p) => ParseP i p a' a b' b m r -> p a' a b' b m r
runParseP p = S.evalStateP [] (unParseP p)
{-# INLINABLE runParseP #-}

{-| Evaluate a non-backtracking parser \'@K@\'leisli arrow, returning the result
    or failing with a 'ParseFailure' exception.
 -}
runParseK
    :: (Monad m, P.Proxy p)
    => (q -> ParseP i p a' a b' b m r) -> (q -> p a' a b' b m r)
runParseK k q = runParseP (k q)
{-# INLINABLE runParseK #-}

-- | Get the internal leftovers buffer
get :: (Monad m, P.Proxy p) => ParseP i p a' a b' b m [Maybe i]
get = ParseP S.get
{-# INLINABLE get #-}

-- | Set the internal leftovers buffer
put :: (Monad m, P.Proxy p) => [Maybe i] -> ParseP i p a' a b' b m ()
put s = ParseP (S.put s)
{-# INLINABLE put #-}
