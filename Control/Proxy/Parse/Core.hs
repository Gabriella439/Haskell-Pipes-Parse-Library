{-| This module defines the core machinery for both backtracking and
    non-backtracking parsers.
-}

{-# LANGUAGE KindSignatures #-}

module Control.Proxy.Parse.Core (
    -- * Backtracking parsers
    ParseT(..),

    -- * Parsing primitives
    draw,
    skip,
    drawN,
    skipN,
    drawWhile,
    skipWhile,
    drawAll,
    skipAll,
    drawIf,
    skipIf,
    endOfInput,
    protect,

    -- * Pushback primitives
    unDraw,
    peek,

    -- ** Diagnostic messages
    parseDebug,
    parseError,
    silence,
    (<?>),

    -- ** Run functions
    runParseT,
    debugParseT,

    -- ** End-of-input utilities
    only,
    onlyK,

    -- * Non-backtracking parsers
{-
    commit,
-}
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
import Data.Foldable (toList)
import Data.Monoid (Monoid(mempty), (<>))
import qualified Data.Sequence as S
import Data.Sequence (ViewL((:<)), (<|), (|>))

{-| Use 'ParseT' to:

    * parse input incrementally,

    * backtrack unlimitedly on failure,

    * return all parsing solutions,

    * interleave side effects with parsing, and

    * diagnose parse failures with a stream of informative error messages.
-}
newtype ParseT (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a m r =
    ParseT { unParseT ::
        StateT (S.Seq (Maybe a)) (
        ErrorT String            (
        P.RespondT p () (Maybe a) (S.Seq (Maybe a))
        m ) ) r }

instance (Monad m, P.Interact p) => Functor (ParseT p a m) where
    fmap f p = ParseT (fmap f (unParseT p))

instance (Monad m, P.Interact p) => Applicative (ParseT p a m) where
    pure r  = ParseT (pure r)
    f <*> x = ParseT (unParseT f <*> unParseT x)

instance (Monad m, P.Interact p) => Monad (ParseT p a m) where
    return r = ParseT (return r)
    m >>= f  = ParseT (unParseT m >>= \r -> unParseT (f r))

instance (P.Interact p) => MonadTrans (ParseT p i) where
    lift m = ParseT (lift (lift (lift m)))

instance (Monad m, P.Interact p) => Alternative (ParseT p a m) where
    empty = ParseT (StateT (\_ -> ErrorT (P.RespondT (
        P.runIdentityP (return mempty) ))))
    p1 <|> p2 = ParseT (StateT (\s -> ErrorT (P.RespondT (
        P.runIdentityP (do
            draw1 <- P.IdentityP (P.runRespondT (runErrorT (
                runStateT (unParseT p1)  s           )))
            draw2 <- P.IdentityP (P.runRespondT (runErrorT (
                runStateT (unParseT p2) (s <> draw1) )))
            return (draw1 <> draw2) ) ))))

instance (Monad m, P.Interact p) => MonadPlus (ParseT p a m) where
    mzero = empty
    mplus = (<|>)

-- | Request a single element
draw :: (Monad m, P.Interact p) => ParseT p a m a
draw = ParseT (StateT (\s -> ErrorT (P.RespondT (P.runIdentityP (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Left "draw: End of input"
                Just a  -> Right (a, s) ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Left "draw: End of input"
            Just a  -> Right (a, mas) ) )))))

-- | Skip a single element
skip :: (Monad m, P.Interact p) => ParseT p a m ()
skip = ParseT (StateT (\s -> ErrorT (P.RespondT (P.runIdentityP (
    case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Left "skip: End of input"
                Just _  -> Right ((), s) ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Left "skip: End of input"
            Just _  -> Right ((), mas) ) )))))

-- | Request a fixed number of elements
drawN :: (Monad m, P.Interact p) => Int -> ParseT p a m (S.Seq a)
drawN n0 = ParseT (StateT (\s0 -> ErrorT (P.RespondT (
    P.runIdentityP (go0 S.empty s0 n0) ))))
  where
    go0 as s n = case compare n 0 of
        GT -> case S.viewl s of
            S.EmptyL -> go1 as n
            ma:<mas  -> case ma of
                Nothing -> err n
                Just a  -> go0 (as |> a) mas $! (n - 1)
        _  -> P.respond (Right (as, s))
    go1 as n = case compare n 0 of
        GT -> do
            ma <- P.request ()
            fmap (ma <|) (case ma of
                Nothing -> err n
                Just a  -> go1 (as |> a) $! (n - 1) )
        _  -> P.respond (Right (as, S.empty))
    err nLeft = P.respond (Left (
        "drawN " ++ show n0 ++ ": Found " ++ show (n0 - nLeft) ++ " elements"))

{-| Skip a fixed number of elements

    Faster than 'drawN' if you don't need the input -}
skipN :: (Monad m, P.Interact p) => Int -> ParseT p a m ()
skipN n0 = ParseT (StateT (\s0 -> ErrorT (P.RespondT (
    P.runIdentityP (go0 s0 n0) ))))
  where
    go0 s n = case compare n 0 of
        GT -> case S.viewl s of
            S.EmptyL -> go1 n
            ma:<mas  -> case ma of
                Nothing -> err n
                Just _  -> go0 mas $! (n - 1)
        _  -> P.respond (Right ((), s))
    go1 n = case compare n 0 of
        GT -> do
            ma <- P.request ()
            fmap (ma <|) (case ma of
                Nothing -> err n
                Just _  -> go1 $! (n - 1) )
        _  -> P.respond (Right ((), S.empty))
    err nLeft = P.respond (Left (
        "skipN " ++ show n0 ++ ": Found " ++ show (n0 - nLeft) ++ " elements"))

-- | Request as many consecutive elements satisfying a predicate as possible
drawWhile :: (Monad m, P.Interact p) => (a -> Bool) -> ParseT p a m (S.Seq a)
drawWhile pred = ParseT (StateT (\s0 -> ErrorT (P.RespondT (
    P.runIdentityP (go0 S.empty s0) ))))
  where
    go0 as s = case S.viewl s of
        S.EmptyL -> go1 as
        ma:<mas  -> case ma of
            Nothing -> P.respond (Right (as, s))
            Just a  ->
                if (pred a)
                    then go0 (as |> a) mas
                    else P.respond (Right (as, s))
    go1 as = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond (Right (as, S.singleton ma))
            Just a  ->
                if (pred a)
                    then go1 (as |> a)
                    else P.respond (Right (as, S.singleton ma)) )

{-| Skip as many consecutive elements satisfying a predicate as possible

    Faster than 'drawWhile' if you don't need the input -}
skipWhile :: (Monad m, P.Interact p) => (a -> Bool) -> ParseT p a m ()
skipWhile pred = ParseT (StateT (\s0 -> ErrorT (P.RespondT (
    P.runIdentityP (go0 s0) ))))
  where
    go0 s = case S.viewl s of
        S.EmptyL -> go1
        ma:<mas  -> case ma of
            Nothing -> P.respond (Right ((), s))
            Just a  ->
                if (pred a)
                    then go0 mas
                    else P.respond (Right ((), s))
    go1 = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond (Right ((), S.singleton ma))
            Just a  ->
                if (pred a)
                    then go1
                    else P.respond (Right ((), S.singleton ma)) )

-- | Request the rest of the input
drawAll :: (Monad m, P.Interact p) => ParseT p a m (S.Seq a)
drawAll = ParseT (StateT (\s0 -> ErrorT (P.RespondT (
    P.runIdentityP (go0 S.empty s0) ))))
  where
    go0 as s = case S.viewl s of
        S.EmptyL -> go1 as
        ma:<mas  -> case ma of
            Nothing -> P.respond (Right (as, s))
            Just a  -> go0 (as |> a) mas
    go1 as = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond (Right (as, S.singleton ma))
            Just a  -> go1 (as |> a) )

{-| Skip the rest of the input

    Faster than 'drawAll' if you don't need the input -}
skipAll :: (Monad m, P.Interact p) => ParseT p a m ()
skipAll = ParseT (StateT (\s0 -> ErrorT (P.RespondT (
    P.runIdentityP (go0 s0) ))))
  where
    go0 s = case S.viewl s of
        S.EmptyL -> go1
        ma:<mas  -> case ma of
            Nothing -> P.respond (Right ((), s))
            Just _  -> go0 mas
    go1 = do
        ma <- P.request ()
        fmap (ma <|) (case ma of
            Nothing -> P.respond (Right ((), S.singleton ma))
            Just _  -> go1 )

-- | Request a single element satisfying a predicate
drawIf :: (Monad m, P.Interact p) => (a -> Bool) -> ParseT p a m a
drawIf pred = ParseT (StateT (\s -> ErrorT (P.RespondT (
    P.runIdentityP (case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Left "drawIf: End of input"
                Just a  ->
                    if (pred a)
                        then Right (a, S.empty)
                        else Left "drawIf: Failed predicate" )) ) ))))

-- | Skip a single element satisfying a predicate
skipIf :: (Monad m, P.Interact p) => (a -> Bool) -> ParseT p a m ()
skipIf pred = ParseT (StateT (\s -> ErrorT (P.RespondT (
    P.runIdentityP (case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Left "skipIf: End of input"
                Just a  ->
                    if (pred a)
                        then Right ((), S.empty)
                        else Left "skipIf: Failed predicate" )) ) ))))

-- | Match end of input without consuming it
endOfInput :: (Monad m, P.Interact p) => ParseT p a m ()
endOfInput = ParseT (StateT (\s -> ErrorT (P.RespondT (
    P.runIdentityP (case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Right ((), S.singleton ma)
                Just a  -> Left "eof: Not end of input" ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Right ((), s)
            Just a  -> Left "eof: Not end of input" ) ) ))))

{-| Protect a parser from failing on end of input by returning 'Nothing' instead

> protect p = (Just <$> p) <|> (Nothing <$ endOfInput)
-}
protect :: (Monad m, P.Interact p) => ParseT p a m r -> ParseT p a m (Maybe r)
protect p = (fmap Just p) <|> (fmap (\_ -> Nothing) endOfInput)

-- | Push back a single element into the leftover buffer
unDraw :: (Monad m, P.Interact p) => a -> ParseT p a m ()
unDraw a = ParseT (StateT (\s -> ErrorT (P.RespondT (
    P.respond (Right ((), Just a <| s)) ))))

{-| Look ahead one element without consuming it

    Faster than 'draw' followed by 'unDraw' -}
peek :: (Monad m, P.Interact p) => ParseT p a m a
peek = ParseT (StateT (\s -> ErrorT (P.RespondT (
    P.runIdentityP (case S.viewl s of
        S.EmptyL -> do
            ma <- P.request ()
            fmap (ma <|) (P.respond (case ma of
                Nothing -> Left "peek: End of input"
                Just a  -> Right (a, S.singleton ma) ))
        ma:<mas  -> P.respond (case ma of
            Nothing -> Left "peek: End of input"
            Just a  -> Right (a, s) ) ) ))))

-- | Emit a diagnostic message and continue parsing
parseDebug :: (Monad m, P.Interact p) => String -> ParseT p a m ()
parseDebug str = parseError str <|> pure ()

-- | Silence all messages emitted from the given parser
silence :: (Monad m, P.Interact p) => ParseT p a m r -> ParseT p a m r
silence p = ParseT (StateT (\s -> ErrorT (P.RespondT (
    P.runRespondT (runErrorT (runStateT (unParseT p) s)) //> noLefts ))))
  where
    noLefts e = P.runIdentityP (case e of
        Left  _  -> return S.empty
        Right rs -> P.respond (Right rs) )

{-| Mark the entry and exit points of the given parser with a 'String' name

> p <?> "My Parser"

    ... produces the following diagnostic messages:

> Enter: My Parser
> ...
> Leave: My Parser

    ('<?>') only emits the latter message if the parser succeeds
-}
(<?>) :: (Monad m, P.Interact p) => ParseT p a m r -> String -> ParseT p a m r
p <?> str = do
    parseDebug ("Enter: " ++ str)
    r <- p
    parseDebug ("Leave: " ++ str)
    return r

{-| Emit a diagnostic message and abort parsing

    Equivalent to @(parseDebug str >> empty)@, but faster -}
parseError :: (Monad m, P.Interact p) => String -> ParseT p a m r
parseError str = ParseT (StateT (\_ -> ErrorT (return (Left str))))

{-| Convert a backtracking parser to a 'Pipe' that incrementally consumes input
    and streams valid parse results -}
runParseT
 :: (Monad m, P.Interact p)
 => ParseT p a m r -> () -> P.Pipe p (Maybe a) r m ()
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
 :: (Monad m, P.Interact p)
 => ParseT p a m r -> () -> P.Pipe p (Maybe a) (Either String r) m ()
debugParseT p () = P.runIdentityP (do
    P.IdentityP (P.runRespondT (runErrorT (runStateT (unParseT p) mempty))) //>
        \x -> do
            P.respond (case x of
                Left   e     -> Left  e
                Right (r, _) -> Right r )
            return mempty
    return () )

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

{-| Wrap a proxy \'@K@\'leisli arrow's output in 'Just' and finish with a
    'Nothing' -}
onlyK
 :: (Monad m, P.Proxy p)
 => (q -> p a' a b' b m r) -> (q -> p a' a b' (Maybe b) m r)
onlyK k q = only (k q)

{-
-- | Convert a backtracking parser to a non-backtracking parser
commit
 :: (Monad m, P.Interact p)
 => ParseT p a m r -> () -> P.Pipe (StateP s (MaybeP p)) (Maybe a) String m r
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
-}

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
