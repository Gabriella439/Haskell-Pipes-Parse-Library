-- | Element-agnostic parsing utilities for @pipes@

{-# LANGUAGE RankNTypes #-}

module Pipes.Parse (
    -- * Parsers
    -- $parser
    Parser,
    draw,
    drawAll,
    unDraw,
    peek,
    isEndOfInput,

    -- * Lenses
    span,
    splitAt,

    -- * Splitters
    groupBy,
    group,
    chunksOf,

    -- * Transformations
    takeFree,
    takeProducers,
    dropFree,

    -- * Joiners
    concat,
    intercalate,

    -- * Re-exports
    -- $reexports
    module Lens.Family2,
    module Lens.Family2.State.Strict,
    module Control.Monad.Trans.Free,
    module Control.Monad.Trans.State.Strict
    ) where

import Control.Monad (join)
import Control.Monad.Trans.Free (
    FreeF(Pure, Free), FreeT(FreeT, runFreeT), transFreeT )
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.State.Strict (
    StateT(StateT, runStateT), evalStateT, execStateT )
import Lens.Family2 (Lens', (^.), over)
import Lens.Family2.State.Strict (zoom)
import Pipes
import Prelude hiding (concat, takeWhile, splitAt, span)

{- $parser
    @pipes-parse@ handles end-of-input and pushback by storing a 'Producer' in
    a 'StateT' layer.
-}

-- | A 'Parser' is an action that reads from and writes to a stored 'Producer'
type Parser a m r = forall x . StateT (Producer a m x) m r

{-| Draw one element from the underlying 'Producer', returning 'Left' if the
    'Producer' is empty
-}
draw :: (Monad m) => Parser a m (Maybe a)
draw = do
    p <- S.get
    x <- lift (next p)
    case x of
        Left   r      -> do
            S.put (return r)
            return Nothing
        Right (a, p') -> do
            S.put p'
            return (Just a)
{-# INLINABLE draw #-}

{-| Draw all elements from the underlying 'Producer'

    Note that 'drawAll' is not an idiomatic use of @pipes-parse@, but I provide
    it for simple testing purposes.  Idiomatic @pipes-parse@ style consumes the
    elements immediately as they are generated instead of loading all elements
    into memory.
-}
drawAll :: (Monad m) => Parser a m [a]
drawAll = go id
  where
    go diffAs = do
        ma <- draw
        case ma of
            Nothing -> return (diffAs [])
            Just a  -> go (diffAs . (a:))
{-# INLINABLE drawAll #-}

-- | Push back an element onto the underlying 'Producer'
unDraw :: (Monad m) => a -> Parser a m ()
unDraw a = S.modify (yield a >>)
{-# INLINABLE unDraw #-}

{-| 'peek' checks the first element of the stream, but uses 'unDraw' to push the
    element back so that it is available for the next 'draw' command.

> peek = do
>     x <- draw
>     case x of
>         Left  _ -> return ()
>         Right a -> unDraw a
>     return x
-}
peek :: (Monad m) => Parser a m (Maybe a)
peek = do
    x <- draw
    case x of
        Nothing -> return ()
        Just a  -> unDraw a
    return x
{-# INLINABLE peek #-}

{-| Check if the underlying 'Producer' is empty

> isEndOfInput = liftM isNothing peek
-}
isEndOfInput :: (Monad m) => Parser a m Bool
isEndOfInput = do
    x <- peek
    return (case x of
        Nothing -> True
        Just _  -> False )
{-# INLINABLE isEndOfInput #-}

{-| 'span' is an improper lens from a 'Producer' to two 'Producer's split using
    the given predicate
-}
span
    :: (Monad m)
    => (a -> Bool) -> Lens' (Producer a m x) (Producer a m (Producer a m x))
span predicate k p0 = fmap join (k (to p0))
  where
--  to :: (Monad m) => Producer a m r -> Producer a m (Producer a m r)
    to p = do
        x <- lift (next p)
        case x of
            Left r        -> return (return r)
            Right (a, p') ->
                if (predicate a)
                then do
                    yield a
                    to p'
                else return (yield a >> p')
{-# INLINABLE span #-}

{-| 'splitAt' is an improper lens from a 'Producer' to two 'Producer's split
    after the given number of elements
-}
splitAt
    :: (Monad m)
    => Int -> Lens' (Producer a m x) (Producer a m (Producer a m x))
splitAt n0 k p0 = fmap join (k (to n0 p0))
  where
--  to :: (Monad m) => Int -> Producer a m r -> Producer a m (Producer a m r)
    to n p =
        if (n <= 0)
        then return p
        else do
            x <- lift (next p)
            case x of
                Left   r      -> return (return r)
                Right (a, p') -> do
                    yield a
                    to (n - 1) p'
{-# INLINABLE splitAt #-}

{-| 'groupBy' is an improper lens from a 'Producer' to a 'FreeT' of 'Producer's
    grouped using the given equality predicate
-}
groupBy
    :: (Monad m)
    => (a -> a -> Bool) -> Lens' (Producer a m x) (FreeT (Producer a m) m x)
groupBy equals k p0 = fmap concat (k (to p0)) -- dimap to (fmap concat)
  where
--  to :: (Monad m) => Producer a m r -> FreeT (Producer a m) m r
    to p = FreeT $ do
        x <- next p
        return $ case x of
            Left   r      -> Pure r
            Right (a, p') -> Free $ do
	        p'' <- (yield a >> p')^.span (equals a)
		return $ to p''
{-# INLINABLE groupBy #-}

-- | Like 'groupBy', where the equality predicate is ('==')
group :: (Monad m, Eq a) => Lens' (Producer a m x) (FreeT (Producer a m) m x)
group = groupBy (==)
{-# INLINABLE group #-}

{-| 'chunksOf' is an improper lens from a 'Producer' to a 'FreeT' of 'Producer's
    of fixed length
-}
chunksOf
    :: (Monad m) => Int -> Lens' (Producer a m x) (FreeT (Producer a m) m x)
chunksOf n0 k p0 = fmap concat (k (to p0))
  where
--  to :: (Monad m) => Producer a m r -> FreeT (Producer a m) m r
    to p = FreeT $ do
        x <- next p
        return $ case x of
            Left   r      -> Pure r
            Right (a, p') -> Free $ do
                p'' <- (yield a >> p')^.splitAt n0
                return (to p'')
{-# INLINABLE chunksOf #-}

-- | Join a 'FreeT'-delimited stream of 'Producer's into a single 'Producer'
concat :: (Monad m) => FreeT (Producer a m) m x -> Producer a m x
concat = go
  where
    go f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                go f'
{-# INLINABLE concat #-}

{-| Join a 'FreeT'-delimited stream of 'Producer's into a single 'Producer' by
    intercalating a 'Producer' in between them
-}
intercalate
    :: (Monad m)
    => Producer a m () -> FreeT (Producer a m) m x -> Producer a m x
intercalate sep = go0
  where
    go0 f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                go1 f'
    go1 f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                sep
                f' <- p
                go1 f'
{-# INLINABLE intercalate #-}

-- | @(takeFree n)@ only keeps the first @n@ functor layers of a 'FreeT'
takeFree :: (Functor f, Monad m) => Int -> FreeT f m () -> FreeT f m ()
takeFree = go
  where
    go n f =
        if (n > 0)
        then FreeT $ do
            x <- runFreeT f
            case x of
                Pure () -> return (Pure ())
                Free w  -> return (Free (fmap (go $! n - 1) w))
        else return ()
{-# INLINABLE takeFree #-}

{-| @(takeProducers n)@ only keeps the first @n@ 'Producer's of a 'FreeT'

    'takeFree'' differs from 'takeFree' by draining unused 'Producer's in order
    to preserve the return value.
-}
takeProducers
    :: (Monad m) => Int -> FreeT (Producer a m) m x -> FreeT (Producer a m) m x
takeProducers = go0
  where
    go0 n f = FreeT $
        if (n > 0)
        then do
            x <- runFreeT f
            return $ case x of
                Pure r -> Pure r
                Free p -> Free $ fmap (go0 $! n - 1) p
        else go1 f
    go1 f = do
        x <- runFreeT f
        case x of
            Pure r -> return (Pure r)
            Free p -> do
                f' <- runEffect (for p discard)
                go1 f'
{-# INLINABLE takeProducers #-}

{-| @(dropFree n)@ peels off the first @n@ 'Producer' layers of a 'FreeT'

    Use carefully: the peeling off is not free.   This runs the first @n@
    layers, just discarding everything they produce.
-}
dropFree
    :: (Monad m) => Int -> FreeT (Producer a m) m x -> FreeT (Producer a m) m x
dropFree = go
  where
    go n ft
        | n <= 0 = ft
        | otherwise = FreeT $ do
            ff <- runFreeT ft
            case ff of
                Pure _ -> return ff
                Free f -> do
                    ft' <- runEffect $ for f discard
                    runFreeT $ go (n-1) ft'
{-# INLINABLE dropFree #-}

{- $reexports
    @Lens.Family2@ re-exports 'Lens'', ('^.') and 'over'.

    @Lens.Family2.State.Strict@ re-exports 'zoom'.

    @Control.Monad.Trans.Free@ re-exports 'FreeF', 'FreeT', 'runFreeT', and
    'transFreeT'.

    @Control.Monad.Trans.State.Strict@ re-exports 'StateT', 'runStateT',
    'evalStateT', and 'execStateT'.
-}
