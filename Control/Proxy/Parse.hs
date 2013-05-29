{-| Parsing utilities for pipes

    This module also provides an orphan 'S.MonadState' instance for 'StateP':

> instance (Monad m, Proxy p) => MonadState s (StateP s p a' a b' b m) where ...
-}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Control.Proxy.Parse (
    -- * Pushback and Leftovers
    -- $pushback
    draw,
    unDraw,

    -- * Utilities
    peek,
    isEndOfInput,
    skipAll,
    passUpTo,
    passWhile,

    -- * Adapters
    -- $adapters
    wrap,
    unwrap,
    fmapPull,
    returnPull,
    bindPull,

    -- * Lenses
    -- $lenses
    zoom,
    _fst,
    _snd,

    -- * Re-exports
    module Control.Proxy.Trans.State
    ) where

import Control.Monad (forever)
import qualified Control.Monad.State.Class as S
import Control.Proxy ((>->), (\>\), (//>), (>\\), (?>=))
import qualified Control.Proxy as P
import Control.Proxy.Trans.State (
    StateP(StateP, unStateP),
    state,
    stateT,
    runStateP,
    runStateK,
    evalStateP,
    evalStateK,
    execStateP,
    execStateK,
    get,
    put,
    modify,
    gets )

instance (Monad m, P.Proxy p) => S.MonadState s (StateP s p a' a b' b m) where
    get = get
    put = put

{- $pushback
    'unDraw' stores all leftovers in a 'StateP' buffer and 'draw' retrieves
    leftovers from this buffer before drawing new input from upstream.
-}

-- | Like @request ()@, except try to use the leftovers buffer first
draw :: (Monad m, P.Proxy p) => StateP [a] p () (Maybe a) y' y m (Maybe a)
draw = do
    s <- get
    case s of
        []   -> P.request ()
        a:as -> do
            put as
            return (Just a)

-- | Push an element back onto the leftovers buffer
unDraw :: (Monad m, P.Proxy p) => a -> StateP [a] p x' x y' y m ()
unDraw a = modify (a:)

-- | Peek at the next element without consuming it
peek :: (Monad m, P.Proxy p) => StateP [a] p () (Maybe a) y' y m (Maybe a)
peek = do
    ma <- draw
    case ma of
        Nothing -> return ()
        Just a  -> unDraw a
    return ma

-- | Check if at end of stream
isEndOfInput :: (Monad m, P.Proxy p) => StateP [a] p () (Maybe a) y' y m Bool
isEndOfInput = do
    ma <- peek
    case ma of
        Nothing -> return True
        Just _  -> return False

-- | Drain all input
skipAll :: (Monad m, P.Proxy p) => () -> StateP [a] p () (Maybe a) y' y m ()
skipAll () = loop
  where
    loop = do
        ma <- draw
        case ma of
            Nothing -> return ()
            Just _  -> loop

-- | Pass up to the specified number of elements
passUpTo
    :: (Monad m, P.Proxy p)
    => Int -> () -> P.Pipe (StateP [a] p) (Maybe a) (Maybe a) m r
passUpTo n0 () = go n0
  where
    go n0 =
        if (n0 <= 0)
        then forever $ P.respond Nothing
        else do
            ma <- draw
            P.respond ma
            case ma of
                Nothing -> forever $ P.respond Nothing
                Just _  -> go (n0 - 1)

-- | Pass as many consecutive elements satisfying a predicate as possible
passWhile
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> () -> P.Pipe (StateP [a] p) (Maybe a) (Maybe a) m r
passWhile pred () = go
  where
    go = do
        ma <- draw
        case ma of
            Nothing -> forever $ P.respond Nothing
            Just a  ->
                if (pred a)
                then do
                    P.respond ma
                    go
                else do
                    unDraw a
                    forever $ P.respond Nothing

{- $adapters
    Use 'wrap' and 'unwrap' to convert between guarded and unguarded pipes.

    'fmapPull', 'returnPull', and 'bindPull' promote compatibility with
    existing utilities that are not 'Maybe' aware.
-}

{-| Guard a pipe from terminating by wrapping every output in 'Just' and ending
    with a never-ending stream of 'Nothing's
-}
wrap :: (Monad m, P.Proxy p) => p a' a b' b m r -> p a' a b' (Maybe b) m s
wrap p = P.runIdentityP $ do
    P.IdentityP p //> \b -> P.respond (Just b)
    forever $ P.respond Nothing

{-| Compose 'unwrap' downstream of a guarded pipe to unwrap all 'Just's and
    terminate on the first 'Nothing'
-}
unwrap :: (Monad m, P.Proxy p) => x -> p x (Maybe a) x a m ()
unwrap x = P.runIdentityP (go x)
  where
    go x = do
        ma <- P.request x
        case ma of
            Nothing -> return ()
            Just a  -> do
                x2 <- P.respond a
                go x2

{-| Lift a 'Maybe'-oblivious pipe to a 'Maybe'-aware pipe by auto-forwarding
    all 'Nothing's

> fmapPull f >-> fmapPull g = fmapPull (f >-> g)
>
> fmapPull pull = pull
-}
fmapPull
    :: (Monad m, P.Proxy p)
    => (x -> p x        a  x        b  m r)
    -> (x -> p x (Maybe a) x (Maybe b) m r)
fmapPull f = bindPull (f >-> returnPull)

-- | Wrap all values in 'Just'
returnPull :: (Monad m, P.Proxy p) => x -> p x a x (Maybe a) m r
returnPull = P.mapD Just

{-| Lift a 'Maybe'-generating pipe to a 'Maybe'-transforming pipe by
    auto-forwarding all 'Nothing's

> -- Using: f >>> g = f >-> bindPull g
>
> returnPull >>> f = f
>
> f >>> returnPull = f
>
> (f >>> g) >>> h = f >>> (g >>> h)

Or equivalently:

> returnPull >-> bindPull f = f
>
> bindPull returnPull = pull
>
> bindPull (f >-> bindPull g) = bindPull f >-> bindPull g
-}
bindPull
    :: (Monad m, P.Proxy p)
    => (x -> p x        a  x (Maybe b) m r)
    -> (x -> p x (Maybe a) x (Maybe b) m r)
bindPull f = P.runIdentityP . (up \>\ P.IdentityP . f)
  where
    up a' = do
        ma <- P.request a'
        case ma of
            Nothing -> do
                a'2 <- P.respond Nothing
                up a'2
            Just a  -> return a

{- $lenses
    Use 'zoom', '_fst', and '_snd' to mix pipes that have different leftover
    buffers or to isolate leftover buffers of different parsing stages.
-}

{-| 'zoom' in on a sub-state using a @Lens@

> zoom :: Lens' s1 s2 -> StateP s2 p a' a b' b m r -> StateP s1 p a' a b' b m r

> zoom (f . g) = zoom f . zoom g
>
> zoom id = id
-}
zoom
    :: (Monad m, P.Proxy p)
    => ((s2 -> (s2, s2)) -> (s1 -> (s2, s1)))
    -- ^ Lens' s1 s2
    -> StateP s2 p a' a b' b m r
    -- ^ Local state
    -> StateP s1 p a' a b' b m r
    -- ^ Global state
zoom lens p = StateP $ \s2_0 ->
    let (s1_0, s2_0') = lens (\x -> (x, x)) s2_0
    in  (up >\\ P.thread_P (unStateP p s1_0) s2_0' //> dn) ?>= nx
  where
    up ((a', s1), s2) =
        let (_, s2') = lens (\x -> (x, s1)) s2
        in  P.request (a', s2') ?>= \(a, s2'') ->
            let (s1', s2''') = lens (\x -> (x, x)) s2''
            in  P.return_P ((a, s1'), s2''')
    dn ((b, s1), s2) =
        let (_, s2') = lens (\x -> (x, s1)) s2
        in  P.respond (b, s2') ?>= \(b', s2'') ->
            let (s1', s2''') = lens (\x -> (x, x)) s2''
            in  P.return_P ((b', s1'), s2''')
    nx ((r, s1), s2) =
        let (_, s2') = lens (\x -> (x, s1)) s2
        in  P.return_P (r, s2')

{-| A lens to the first element of a pair

    Like @_1@, but more monomorphic

> _fst :: Lens' (a, b) a
-}
_fst :: (Functor f) => (a -> f b) -> ((a, x) -> f (b, x))
_fst f (a, x) = fmap (\b -> (b, x)) (f a)

{-| A lens to the second element of a pair

    Like @_2@, but more monomorphic

> _snd :: Lens' (a, b) b
-}
_snd :: (Functor f) => (a -> f b) -> ((x, a) -> f (x, b))
_snd f (x, a) = fmap (\b -> (x, b)) (f a)
