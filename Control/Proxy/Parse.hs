-- | Core primitives for parsing

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Control.Proxy.Parse (
    -- * Leftovers
    draw,
    unDraw,

    -- * Utilities
    drawIt,
    peek,
    isEndOfInput,
    skipAll,
    passUpToN,
    passWhile,

    -- * Parse Failures
    ParseFailure(..),

    -- * Adapters
    wrap,
    unwrap,
    fmapPull,
    returnPull,
    bindPull,

    -- * Lenses
    zoom,
    _fst,
    _snd,
    (/\),

    -- * Re-exports
    module Control.Proxy.Trans.State
    ) where

import Control.Exception (Exception(toException), SomeException)
import qualified Control.Monad.State.Class as S
import Control.Proxy
import Control.Proxy.Trans.State
import Control.Proxy.Trans.Either (EitherP, throw)
import Data.Typeable (Typeable)

instance (Monad m, Proxy p) => S.MonadState s (StateP s p a' a b' b m) where
    get = get
    put = put

-- | Like @request ()@, except try to use the leftovers buffer first.
--
-- A 'Nothing' return value indicates end of input.
draw :: (Monad m, Proxy p) => StateP [Maybe a] p () (Maybe a) y' y m (Maybe a)
draw = do
    s <- get
    case s of
        []   -> request ()
        ma:mas -> do
            put mas
            return ma

-- | Prepend an element to the leftovers buffer.
unDraw :: (Monad m, Proxy p) => a -> StateP [Maybe a] p x' x y' y m ()
unDraw a = modify (Just a:)

-- | Try to 'draw' an element. If there is no more input available, throw a
-- 'ParseFailure' exception in `EitherP`.
drawIt
    :: (Monad m, Proxy p)
    => StateP [Maybe a] (EitherP SomeException p) () (Maybe a) y' y m a
drawIt = do
    ma <- draw
    case ma of
        Nothing -> liftP $ throw $ toException $
            ParseFailure "drawIt: End of input"
        Just a  -> return a

-- | Peek at the next input element without consuming it.
peek :: (Monad m, Proxy p) => StateP [Maybe a] p () (Maybe a) y' y m (Maybe a)
peek = do
    ma <- draw
    case ma of
        Nothing -> return ()
        Just a  -> unDraw a
    return ma

-- | Check if at end of input stream.
isEndOfInput
    :: (Monad m, Proxy p) => StateP [Maybe a] p () (Maybe a) y' y m Bool
isEndOfInput = do
    ma <- peek
    case ma of
        Nothing -> return True
        Just _  -> return False

-- | Consume and discard all input.
skipAll :: (Monad m, Proxy p) => () -> StateP [Maybe a] p () (Maybe a) y' y m ()
skipAll () = loop
  where
    loop = do
        ma <- draw
        case ma of
            Nothing -> return ()
            Just _  -> loop

-- | Forward downstream up to the specified number of elements.
passUpToN
    :: (Monad m, Proxy p)
    => Int -> () -> StateP [Maybe a] p () (Maybe a) () (Maybe a) m r
passUpToN n0 () = go n0
  where
    go n0 =
        if (n0 <= 0)
        then forever $ respond Nothing
        else do
            ma <- draw
            respond ma
            case ma of
                Nothing -> forever $ respond Nothing
                Just _  -> go (n0 - 1)

-- | Forward downstream as many consecutive elements satisfying a predicate as
-- possible.
passWhile
    :: (Monad m, Proxy p)
    => (a -> Bool) -> () -> StateP [Maybe a] p () (Maybe a) () (Maybe a) m r
passWhile pred () = go
  where
    go = do
        ma <- draw
        case ma of
            Nothing -> forever $ respond Nothing
            Just a  ->
                if (pred a)
                then do
                    respond ma
                    go
                else do
                    unDraw a
                    forever $ respond Nothing

-- | Parsing failed. The 'String' describes the nature of the parse failure
newtype ParseFailure = ParseFailure String deriving (Show, Typeable)

instance Exception ParseFailure

{-| Guard a pipe from terminating by wrapping every output in 'Just' and ending
    with a never-ending stream of 'Nothing's.
-}
wrap :: (Monad m, Proxy p) => p a' a b' b m r -> p a' a b' (Maybe b) m s
wrap p = runIdentityP $ do
    IdentityP p //> \b -> respond (Just b)
    forever $ respond Nothing

{-| Compose 'unwrap' downstream of a guarded pipe to unwrap all 'Just's and
    terminate on the first 'Nothing'.
-}
unwrap :: (Monad m, Proxy p) => x -> p x (Maybe a) x a m ()
unwrap x = runIdentityP (go x)
  where
    go x = do
        ma <- request x
        case ma of
            Nothing -> return ()
            Just a  -> do
                x2 <- respond a
                go x2

{-| Lift a 'Maybe'-oblivious pipe to a 'Maybe'-aware pipe by auto-forwarding
    all 'Nothing's.

> fmapPull f >-> fmapPull g = fmapPull (f >-> g)
>
> fmapPull pull = pull
-}
fmapPull
    :: (Monad m, Proxy p)
    => (x -> p x        a  x        b  m r)
    -> (x -> p x (Maybe a) x (Maybe b) m r)
fmapPull f = bindPull (f >-> returnPull)

-- | Wrap all values flowing downstream in 'Just'.
returnPull :: (Monad m, Proxy p) => x -> p x a x (Maybe a) m r
returnPull = mapD Just

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
    :: (Monad m, Proxy p)
    => (x -> p x        a  x (Maybe b) m r)
    -> (x -> p x (Maybe a) x (Maybe b) m r)
bindPull f = runIdentityP . (up \>\ IdentityP . f)
  where
    up a' = do
        ma <- request a'
        case ma of
            Nothing -> do
                a'2 <- respond Nothing
                up a'2
            Just a  -> return a

{-| 'zoom' in on a sub-state using a 'Control.Lens.Lens'.

> zoom :: Lens' s1 s2 -> StateP s2 p a' a b' b m r -> StateP s1 p a' a b' b m r

> zoom (f . g) = zoom f . zoom g
>
> zoom id = id
-}
zoom
    :: (Monad m, Proxy p)
    => ((s2 -> (s2, s2)) -> (s1 -> (s2, s1)))
    -- ^ 'Control.Lens.Lens'' s1 s2
    -> StateP s2 p a' a b' b m r
    -- ^ Local state
    -> StateP s1 p a' a b' b m r
    -- ^ Global state
zoom lens p = StateP $ \s2_0 ->
    let (s1_0, s2_0') = lens (\x -> (x, x)) s2_0
    in  (up >\\ thread_P (unStateP p s1_0) s2_0' //> dn) ?>= nx
  where
    up ((a', s1), s2) =
        let (_, s2') = lens (\x -> (x, s1)) s2
        in  request (a', s2') ?>= \(a, s2'') ->
            let (s1', s2''') = lens (\x -> (x, x)) s2''
            in  return_P ((a, s1'), s2''')
    dn ((b, s1), s2) =
        let (_, s2') = lens (\x -> (x, s1)) s2
        in  respond (b, s2') ?>= \(b', s2'') ->
            let (s1', s2''') = lens (\x -> (x, x)) s2''
            in  return_P ((b', s1'), s2''')
    nx ((r, s1), s2) =
        let (_, s2') = lens (\x -> (x, s1)) s2
        in  return_P (r, s2')

{-| A 'Control.Lens.Lens' to the first element of a pair.

    Like @_1@, but more monomorphic

> _fst :: Lens' (a, b) a
-}
_fst :: (Functor f) => (a -> f b) -> ((a, x) -> f (b, x))
_fst f (a, x) = fmap (\b -> (b, x)) (f a)

{-| A 'Control.Lens.Lens' to the second element of a pair.

    Like @_2@, but more monomorphic

> _snd :: Lens' (a, b) b
-}
_snd :: (Functor f) => (a -> f b) -> ((x, a) -> f (x, b))
_snd f (x, a) = fmap (\b -> (x, b)) (f a)

{-| Pair up two 'Control.Lens.Lens'es.

> (/\) :: Lens' c a -> Lens' c b -> Lens' c (a, b)

> _fst /\ _snd = id
-}
(/\)
    :: (Functor f)
    => ((a -> (a, a)) -> (c -> (a, c)))
    -- ^ 'Control.Lens.Lens'' c a
    -> ((b -> (b, b)) -> (c -> (b, c)))
    -- ^ 'Control.Lens.Lens'' c b
    -> (((a, b) -> f (a, b)) -> (c -> f c))
    -- ^ 'Control.Lens.Lens'' c (a, b)
(lens1 /\ lens2) f c0 =
    let (a, _) = lens1 (\a_ -> (a_, a_)) c0
        (b, _) = lens2 (\b_ -> (b_, b_)) c0
        fab = f (a, b)
    in  fmap (\(a, b) ->
            let (_, c1) = lens1 (\a_ -> (a_, a)) c0
                (_, c2) = lens2 (\b_ -> (b_, b)) c1
            in  c2
            ) fab

infixl 7 /\
