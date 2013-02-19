{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Parse (
    ParseT(..),
    parseError,
    parseDebug,
    runParseT,
    debugParseT,
    commit,
    File(..),
    eof,
    eofK
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Trans.Either (EitherT(EitherT, runEitherT))
import Control.Proxy
import Control.Proxy.Trans.Maybe
import Control.Proxy.Trans.State
import Data.Monoid (Monoid(mempty, mappend), (<>))

newtype ParseT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) s m r =
    ParseT { unParseT :: StateT s (EitherT String (RespondT p () s s m)) r }
{-                              ^                                ^ ^
                                |                                | |
                Leftover Input -+                    More input -+ |
                                                                   |
                                                      Input drawn -+  -}

instance (Monad m, Interact p) => Functor (ParseT p s m) where
    fmap f p = ParseT (fmap f (unParseT p))

instance (Monad m, Interact p) => Applicative (ParseT p s m) where
    pure r  = ParseT (pure r)
    f <*> x = ParseT (unParseT f <*> unParseT x)

instance (Monad m, Interact p) => Monad (ParseT p s m) where
    return r = ParseT (return r)
    m >>= f  = ParseT (unParseT m >>= \r -> unParseT (f r))

instance (Interact p) => MonadTrans (ParseT p s) where
    lift m = ParseT (lift (lift (lift m)))

instance (Monad m, Interact p, Monoid s) => Alternative (ParseT p s m) where
    empty = ParseT (StateT (\_ -> EitherT (RespondT (
        runIdentityP (return mempty) ))))
    p1 <|> p2 = ParseT (StateT (\s -> EitherT (RespondT (runIdentityP (do
        draw  <- IdentityP (
            runRespondT (runEitherT (runStateT (unParseT p1)  s         )))
        draw' <- IdentityP (
            runRespondT (runEitherT (runStateT (unParseT p2) (s <> draw))))
        return (draw <> draw') )))))

instance (Monad m, Interact p, Monoid s) => MonadPlus (ParseT p s m) where
    mzero = empty
    mplus = (<|>)

parseError :: (Monad m, Interact p) => String -> ParseT p s m r
parseError str = ParseT (StateT (\s -> EitherT (RespondT (runIdentityP (do
    respond (Left str) )))))

parseDebug :: (Monad m, Interact p, Monoid s) => String -> ParseT p s m ()
parseDebug str = parseError str <|> pure ()

runParseT
 :: (Monad m, Interact p, Monoid s) => ParseT p s m r -> () -> Pipe p s r m ()
runParseT p () = runIdentityP (do
    IdentityP (runRespondT (runEitherT (runStateT (unParseT p) mempty))) //>
        \x -> do
            case x of
                Left   _     -> return mempty
                Right (r, _) -> do
                    respond r
                    return mempty
    return () )

debugParseT
 :: (Monad m, Interact p, Monoid s)
 => ParseT p s m r -> () -> Pipe p s (Either String r) m ()
debugParseT p () = runIdentityP (do
    IdentityP (runRespondT (runEitherT (runStateT (unParseT p) mempty))) //>
        \x -> do
            respond (case x of
                Left   e     -> Left  e
                Right (r, _) -> Right r)
            return mempty
    return () )

commit
 :: (Monad m, Interact p, Monoid s)
 => ParseT p s m r -> () -> Pipe (StateP s (MaybeP p)) s String m r
commit p () = StateP $ \s ->
    (do liftP (runRespondT (runEitherT (runStateT (unParseT p) s)) //> \x ->
            runIdentityP (do
                respond x
                return mempty ) )
        nothing ) >>~ firstSuccess
  where
    firstSuccess a = do
        case a of
            Left  b -> do
                b' <- respond b
                a2 <- request b'
                firstSuccess a
            Right rs -> return rs

data File s = Chunk !s !(File s) | EOF !(File s) | Done deriving (Show)

instance Monoid (File s) where
    mempty = Done
    mappend f1 f2 = go f1 where
        go f = case f of
            Chunk s f' -> Chunk s (go f')
            EOF     f' -> EOF     (go f')
            Done       -> f2

eof :: (Monad m, Proxy p) => p a' a b' b m r -> p a' a b' (File b) m r
eof p = runIdentityP (do
    r <- IdentityP p >>~ wrap
    respond (EOF Done)
    return r )
  where
    wrap a = do
        a' <- respond (Chunk a Done)
        a2 <- request a'
        wrap a2

eofK
 :: (Monad m, Proxy p)
 => (q -> p a' a b' b m r) -> (q -> p a' a b' (File b) m r)
eofK = (eof .)
