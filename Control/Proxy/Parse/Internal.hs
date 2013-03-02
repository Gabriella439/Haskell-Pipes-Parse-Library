{-| This module exposes internal implementation details that might change in the
    future.  I only expose this so that people can write high-efficiency parsing
    primitives not implementable in terms of existing primitives. -}

module Control.Proxy.Parse.Internal (
    -- * Backtracking parser
    ParseT(..),

    -- * Non-backtracking parser
    ParseP(..),

    -- * End of input utilities
    only,
    onlyK
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Monad.Trans.State.Strict (StateT(StateT, runStateT))
import qualified Control.Proxy as P
import Control.Proxy ((->>), (>>~), (?>=))
import Control.Proxy.Trans.Codensity(CodensityP)
import Control.Proxy.Trans.Either (EitherP)
import Control.Proxy.Trans.State (StateP)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S

{-| Use 'ParseT' to:

    * parse input incrementally,

    * backtrack unlimitedly on failure,

    * return all parsing solutions, and

    * interleave side effects with parsing.
-}
newtype ParseT p a m r = ParseT
    { unParseT ::
        StateT (Seq (Maybe a)) (
            P.RespondT (CodensityP p) () (Maybe a) (Seq (Maybe a)) m) r
    }
{- To understand the ParseT type, begin from a Hutton-Meijer monadic parser:

> StateT leftovers [] r

   Now replace the list monad with "ListT" (i.e. ProduceT) so that you now have
   a monad transformer:

> StateT leftovers (ProduceT m) r

   Now generalize 'ProduceT' to 'RespondT', for two reasons:

   * Allow requests for more 'input'

   * Return 'drawn' input so that backtracking can reuse it as leftovers

> StateT leftovers (RespondT p () input drawn m) r

   Finally, layer CodensityP over the base proxy so that you don't pay a
   quadratic time complexity for backtracking:

> StateT leftovers (RespondT (CodensityP p) () input drawn m) r
-}

{- NOTE: Deriving the type class instances from the monad transformer stack
         produces code that is just as efficient as hand-writing the logic.
         However, high-efficiency parsing primitives still require unwrapping
         the monad transformer and hand-writing logic at the lowest layer.

         I still like to keep the monad transformer stack in order to document
         how the non-backtracking parser works, even if I don't use it to
         abstract away very much. -}

-- Deriving Functor
instance (Monad m, P.ListT p) => Functor (ParseT p a m) where
    fmap f p = ParseT (fmap f (unParseT p))

-- Deriving Applicative
instance (Monad m, P.ListT p) => Applicative (ParseT p a m) where
    pure r  = ParseT (pure r)
    f <*> x = ParseT (unParseT f <*> unParseT x)

-- Deriving Monad
instance (Monad m, P.ListT p) => Monad (ParseT p a m) where
    return r = ParseT (return r)
    m >>= f  = ParseT (unParseT m >>= \r -> unParseT (f r))

-- Deriving MonadIO
instance (MonadIO m, P.ListT p) => MonadIO (ParseT p a m) where
    liftIO m = ParseT (liftIO m)

instance (P.ListT p) => MonadTrans (ParseT p i) where
    lift m = ParseT (lift (lift m))

-- NOT deriving Alternative
instance (Monad m, P.ListT p) => Alternative (ParseT p a m) where
    empty = ParseT (StateT (\_ -> P.RespondT (return S.empty)))
    p1 <|> p2 = ParseT (StateT (\s -> P.RespondT (do
        draw1 <- P.runRespondT (runStateT (unParseT p1)  s          )
        draw2 <- P.runRespondT (runStateT (unParseT p2) (s >< draw1))
        return (draw1 >< draw2) )))
{- Backtracking reuses drawn input from the first branch so that the second
   branch begins from the correct leftover state.  If you omit all the newtypes,
   the above methods are equivalent to:

   empty _ = return mempty

   (p1 <|> p2) s = do
       draw1 <- p1  s
       draw2 <- p2 (s <> draw)
       return (draw1 <> draw2)

   ... and it's simple to show that those definitions satisfy the Alternative
   laws:

   p <|> empty = p

   empty <|> p = p

   (p1 <|> p2) <|> p3 = p1 <|> (p2 <|> p3)
-}

instance (Monad m, P.ListT p) => MonadPlus (ParseT p a m) where
    mzero = empty
    mplus = (<|>)

{-| Use 'ParseP' for parsing if you want to:

    * stream input in as little memory as possible,

    * request input lazily and incrementally,

    * diagnose parse failures with error messages, and

    * interleave side effects with parsing.
-}
newtype ParseP i p a' a b' b m r = ParseP { unParseP ::
    StateP (Seq (Maybe i)) (EitherP String (CodensityP p)) a' a b' b m r }

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

-- Deriving Alternative
instance (Monad m, P.Proxy p) => Alternative (ParseP i p a' a b' b m) where
    empty = mzero
    (<|>) = mplus

-- Deriving MonadPlus
instance (Monad m, P.Proxy p) => MonadPlus (ParseP i p a' a b' b m) where
    mzero = P.mzero_P
    mplus = P.mplus_P

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

-- Deriving MonadPlusP
instance (P.Proxy p) => P.MonadPlusP (ParseP i p) where
    mzero_P       = ParseP P.mzero_P
    mplus_P p1 p2 = ParseP (P.mplus_P (unParseP p1) (unParseP p2))

instance P.ProxyTrans (ParseP i) where
    liftP p = ParseP (P.liftP (P.liftP (P.liftP p)))

instance P.PFunctor (ParseP i) where
    hoistP nat p = ParseP (P.hoistP (P.hoistP (P.hoistP nat)) (unParseP p))

-- | Wrap a proxy's output in 'Just' and finish with a 'Nothing'
only :: (Monad m, P.Proxy p) => p a' a b' b m r -> p a' a b' (Maybe b) m r
only p = P.runIdentityP (do
    r <- P.IdentityP p >>~ wrap
    P.respond Nothing
    return r )
  where
    wrap a = do
        a' <- P.respond (Just a)
        a2 <- P.request a'
        wrap a2
{-# INLINABLE only #-}

{-| Wrap a proxy \'@K@\'leisli arrow's output in 'Just' and finish with a
    'Nothing' -}
onlyK
 :: (Monad m, P.Proxy p)
 => (q -> p a' a b' b m r) -> (q -> p a' a b' (Maybe b) m r)
onlyK k q = only (k q)
{-# INLINABLE onlyK #-}
