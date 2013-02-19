{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Parse (
    ParseT(..),
    parseError,
    parseDebug,
    runParseT,
    debugParseT,
    commit
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.State.Strict (StateT(StateT, runStateT))
import Control.Monad.Trans.Either (EitherT(EitherT, runEitherT))
import qualified Control.Proxy as P
import Control.Proxy ((>>~), (//>))
import Control.Proxy.Trans.Maybe (MaybeP, nothing)
import Control.Proxy.Trans.State (StateP(StateP))
import Data.Monoid (Monoid(mempty, mappend), (<>))

{- Use 'ParseT' to:

    * parse input incrementally,

    * backtrack on failure,

    * returns /all/ parsing solutions

    * interleave side effects with parsing

    * diagnose parse failures with a stream of informative error messages
-}
newtype ParseT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) s m r =
    ParseT { unParseT :: StateT s (EitherT String (P.RespondT p () s s m)) r }
{-                              ^                                  ^ ^
                                |                                  | |
                Leftover input -+              Request more input -+ |
                                                                     |
                                                  Total input drawn -+  -}

instance (Monad m, P.Interact p) => Functor (ParseT p s m) where
    fmap f p = ParseT (fmap f (unParseT p))

instance (Monad m, P.Interact p) => Applicative (ParseT p s m) where
    pure r  = ParseT (pure r)
    f <*> x = ParseT (unParseT f <*> unParseT x)

instance (Monad m, P.Interact p) => Monad (ParseT p s m) where
    return r = ParseT (return r)
    m >>= f  = ParseT (unParseT m >>= \r -> unParseT (f r))

instance (P.Interact p) => MonadTrans (ParseT p s) where
    lift m = ParseT (lift (lift (lift m)))

instance (Monad m, P.Interact p, Monoid s) => Alternative (ParseT p s m) where
    empty = ParseT (StateT (\_ -> EitherT (P.RespondT (
        P.runIdentityP (return mempty) ))))
    p1 <|> p2 = ParseT (StateT (\s -> EitherT (P.RespondT (P.runIdentityP (do
        draw  <- P.IdentityP (
            P.runRespondT (runEitherT (runStateT (unParseT p1)  s         )))
        draw' <- P.IdentityP (
            P.runRespondT (runEitherT (runStateT (unParseT p2) (s <> draw))))
        return (draw <> draw') )))))

instance (Monad m, P.Interact p, Monoid s) => MonadPlus (ParseT p s m) where
    mzero = empty
    mplus = (<|>)

parseError :: (Monad m, P.Interact p) => String -> ParseT p s m r
parseError str = ParseT (StateT (\_ -> EitherT (return (Left str))))

parseDebug :: (Monad m, P.Interact p, Monoid s) => String -> ParseT p s m ()
parseDebug str = parseError str <|> pure ()

runParseT
 :: (Monad m, P.Interact p, Monoid s)
 => ParseT p s m r -> () -> P.Pipe p s r m ()
runParseT p () = P.runIdentityP (do
    P.IdentityP (P.runRespondT (runEitherT (runStateT (unParseT p) mempty))) //>
        \x -> do
            case x of
                Left   _     -> return mempty
                Right (r, _) -> do
                    P.respond r
                    return mempty
    return () )

debugParseT
 :: (Monad m, P.Interact p, Monoid s)
 => ParseT p s m r -> () -> P.Pipe p s (Either String r) m ()
debugParseT p () = P.runIdentityP (do
    P.IdentityP (P.runRespondT (runEitherT (runStateT (unParseT p) mempty))) //>
        \x -> do
            P.respond (case x of
                Left   e     -> Left  e
                Right (r, _) -> Right r)
            return mempty
    return () )

commit
 :: (Monad m, P.Interact p, Monoid s)
 => ParseT p s m r -> () -> P.Pipe (StateP s (MaybeP p)) s String m r
commit p () = StateP $ \s ->
    (do P.liftP (P.runRespondT (runEitherT (runStateT (unParseT p) s)) //> \x ->
            P.runIdentityP (do
                P.respond x
                return mempty ) )
        nothing ) >>~ firstSuccess
  where
    firstSuccess a = do
        case a of
            Left  b -> do
                b' <- P.respond b
                a2 <- P.request b'
                firstSuccess a
            Right rs -> return rs
