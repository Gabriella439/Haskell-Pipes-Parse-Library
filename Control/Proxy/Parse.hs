{-| This module defines the core machinery for both backtracking and
    non-backtracking parsers.
-}

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Parse (
    -- * Backtracking parsers
    ParseT(..),

    -- ** Diagnostic messages
    parseDebug,
    parseError,

    -- ** Run functions
    runParseT,
    debugParseT,

    -- * Non-backtracking parsers
    commit,
    -- ** Run functions
    runParse,
    evalParse,
    runParseK,
    evalParseK
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State.Strict (StateT(StateT, runStateT))
import Control.Monad.Trans.Error (ErrorT(ErrorT, runErrorT))
import qualified Control.Proxy as P
import Control.Proxy ((>>~), (//>))
import Control.Proxy.Trans.Maybe (MaybeP, nothing)
import Control.Proxy.Trans.State (
    StateP(StateP), runStateP, evalStateP, runStateK, evalStateK )
import Data.Monoid (Monoid(mempty), (<>))

{-| Use 'ParseT' to:

    * parse input incrementally,

    * backtrack on failure,

    * return all parsing solutions,

    * interleave side effects with parsing, and

    * diagnose parse failures with a stream of informative error messages.
-}
newtype ParseT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) s i m r =
    ParseT { unParseT :: StateT s (ErrorT String (P.RespondT p () i s m)) r }
{-                              ^                                 ^ ^
                                |                                 | |
                Leftover input -+             Request more input -+ |
                                                                    |
                                                 Total input drawn -+  -}

instance (Monad m, P.Interact p) => Functor (ParseT p s i m) where
    fmap f p = ParseT (fmap f (unParseT p))

instance (Monad m, P.Interact p) => Applicative (ParseT p s i m) where
    pure r  = ParseT (pure r)
    f <*> x = ParseT (unParseT f <*> unParseT x)

instance (Monad m, P.Interact p) => Monad (ParseT p s i m) where
    return r = ParseT (return r)
    m >>= f  = ParseT (unParseT m >>= \r -> unParseT (f r))

instance (P.Interact p) => MonadTrans (ParseT p s i) where
    lift m = ParseT (lift (lift (lift m)))

instance (Monad m, P.Interact p, Monoid s) => Alternative (ParseT p s i m) where
    empty = ParseT (StateT (\_ -> ErrorT (P.RespondT (
        P.runIdentityP (return mempty) ))))
    p1 <|> p2 = ParseT (StateT (\s -> ErrorT (P.RespondT (P.runIdentityP (do
        draw  <- P.IdentityP (
            P.runRespondT (runErrorT (runStateT (unParseT p1)  s         )))
        draw' <- P.IdentityP (
            P.runRespondT (runErrorT (runStateT (unParseT p2) (s <> draw))))
        return (draw <> draw') )))))

instance (Monad m, P.Interact p, Monoid s) => MonadPlus (ParseT p s i m) where
    mzero = empty
    mplus = (<|>)

-- | Emit a diagnostic message and continue parsing
parseDebug :: (Monad m, P.Interact p, Monoid s) => String -> ParseT p s i m ()
parseDebug str = parseError str <|> pure ()

{-| Emit a diagnostic message and abort parsing

    Equivalent to @parseDebug str >> empty@, but faster -}
parseError :: (Monad m, P.Interact p) => String -> ParseT p s i m r
parseError str = ParseT (StateT (\_ -> ErrorT (return (Left str))))

{-| Convert a backtracking parser to a 'Pipe' that incrementally consumes input
    and streams valid parse results -}
runParseT
 :: (Monad m, P.Interact p, Monoid s)
 => ParseT p s i m r -> () -> P.Pipe p i r m ()
runParseT p () = P.runIdentityP (do
    P.IdentityP (P.runRespondT (runErrorT (runStateT (unParseT p) mempty))) //>
        \x -> do
            case x of
                Left   _     -> return mempty
                Right (r, _) -> do
                    P.respond r
                    return mempty
    return () )

{-| Convert a backtracking parser to a 'Pipe' that incrementally consumes input
    and streams both valid parse results and diagnostic messages -}
debugParseT
 :: (Monad m, P.Interact p, Monoid s)
 => ParseT p s i m r -> () -> P.Pipe p i (Either String r) m ()
debugParseT p () = P.runIdentityP (do
    P.IdentityP (P.runRespondT (runErrorT (runStateT (unParseT p) mempty))) //>
        \x -> do
            P.respond (case x of
                Left   e     -> Left  e
                Right (r, _) -> Right r)
            return mempty
    return () )

-- | Convert a backtracking parser to a non-backtracking parser
commit
 :: (Monad m, P.Interact p, Monoid s)
 => ParseT p s i m r -> () -> P.Pipe (StateP s (MaybeP p)) i String m r
commit p () = StateP $ \s ->
    (do P.liftP (P.runRespondT (runErrorT (runStateT (unParseT p) s)) //> \x ->
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

{-| Initiate a non-backtracking parser with an empty input, returning the result
    and unconsumed input -}
runParse :: (Monoid s) => StateP s p a' a b' b m r -> p a' a b' b m (r, s)
runParse = runStateP mempty

{-| Initiate a non-backtracking parser with an empty input, returning only the
    result -}
evalParse
 :: (Monad m, P.Proxy p, Monoid s)
 => StateP s p a' a b' b m r -> p a' a b' b m r
evalParse = evalStateP mempty

{-| Initiate a non-backtracking parser \'@K@\'leisli arrow with an empty input,
    returning the result and unconsumed input -}
runParseK
 :: (Monoid s) => (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m (r, s))
runParseK = runStateK mempty

{-| Initiate a non-backtracking parser \'@K@\'leisli arrow with an empty input,
    returning only the result -}
evalParseK
 :: (Monad m, P.Proxy p, Monoid s)
 => (q -> StateP s p a' a b' b m r) -> (q -> p a' a b' b m r)
evalParseK = evalStateK mempty
