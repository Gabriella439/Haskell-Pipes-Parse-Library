{-| Element-agnostic parsing utilities for @pipes@

    See "Pipes.Parse.Tutorial" for an extended tutorial
-}

{-# LANGUAGE RankNTypes #-}

module Pipes.Parse (
    -- * Parsing
    -- $parsing
    Parser,
    draw,
    skip,
    drawAll,
    skipAll,
    unDraw,
    peek,
    isEndOfInput,
    foldAll,
    foldAllM,

    -- * Parsing Lenses
    -- $parsinglenses
    span,
    splitAt,
    groupBy,
    group,

    -- * FreeT Lenses
    groupsBy,
    groups,
    chunksOf,

    -- * FreeT Transformations
    takes,
    takes',
    drops,

    -- * FreeT Joiners
    concats,
    intercalates,

    -- * FreeT Folds
    folds,
    foldsM,

    -- * Re-exports
    -- $reexports
    module Control.Monad.Trans.Class,
    module Control.Monad.Trans.Free,
    module Control.Monad.Trans.State.Strict,
    module Pipes
    ) where

import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free (
    FreeF(Pure, Free), FreeT(FreeT, runFreeT), transFreeT )
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.State.Strict (
    StateT(StateT, runStateT), evalStateT, execStateT )
import Data.Functor.Constant (Constant(Constant, getConstant))
import Pipes (Producer, yield, next)
import qualified Pipes as P

import Prelude hiding (span, splitAt)

{- $parsing
    @pipes-parse@ handles end-of-input and pushback by storing a 'Producer' in
    a 'StateT' layer.

    Connect 'Parser's to 'Producer's using either 'runStateT', 'evalStateT', or
    'execStateT':

> runStateT  :: Parser a m r -> Producer a m x -> m (r, Producer a m x)
> evalStateT :: Parser a m r -> Producer a m x -> m  r
> execStateT :: Parser a m r -> Producer a m x -> m    (Producer a m x)
>                                                       ^^^^^^^^^^^^^^
>                                                          Leftovers
-}

-- | A 'Parser' is an action that reads from and writes to a stored 'Producer'
type Parser a m r = forall x . StateT (Producer a m x) m r

{-| Draw one element from the underlying 'Producer', returning 'Nothing' if the
    'Producer' is empty
-}
draw :: Monad m => Parser a m (Maybe a)
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

{-| Skip one element from the underlying 'Producer', returning 'True' if
    successful or 'False' if the 'Producer' is empty

> skip = fmap isJust draw
-}
skip :: Monad m => Parser a m Bool
skip = do
    x <- draw
    return $ case x of
        Nothing -> False
        Just _  -> True
{-# INLINABLE skip #-}

{-| Draw all elements from the underlying 'Producer'

    Note that 'drawAll' is not an idiomatic use of @pipes-parse@, but I provide
    it for simple testing purposes.  Idiomatic @pipes-parse@ style consumes the
    elements immediately as they are generated instead of loading all elements
    into memory.
-}
drawAll :: Monad m => Parser a m [a]
drawAll = go id
  where
    go diffAs = do
        x <- draw
        case x of
            Nothing -> return (diffAs [])
            Just a  -> go (diffAs . (a:))
{-# INLINABLE drawAll #-}

-- | Drain all elements from the underlying 'Producer'
skipAll :: Monad m => Parser a m ()
skipAll = go
  where
    go = do
        x <- draw
        case x  of
            Nothing -> return ()
            Just _  -> go
{-# INLINABLE skipAll #-}

-- | Push back an element onto the underlying 'Producer'
unDraw :: Monad m => a -> Parser a m ()
unDraw a = S.modify (yield a >>)
{-# INLINABLE unDraw #-}

{-| 'peek' checks the first element of the stream, but uses 'unDraw' to push the
    element back so that it is available for the next 'draw' command.

> peek = do
>     x <- draw
>     case x of
>         Nothing -> return ()
>         Just a  -> unDraw a
>     return x
-}
peek :: Monad m => Parser a m (Maybe a)
peek = do
    x <- draw
    case x of
        Nothing -> return ()
        Just a  -> unDraw a
    return x
{-# INLINABLE peek #-}

{-| Check if the underlying 'Producer' is empty

> isEndOfInput = fmap isNothing peek
-}
isEndOfInput :: Monad m => Parser a m Bool
isEndOfInput = do
    x <- peek
    return (case x of
        Nothing -> True
        Just _  -> False )
{-# INLINABLE isEndOfInput #-}

{-| Fold all input values

@
 'Control.Foldl.purely' foldAll :: Monad m => 'Control.Foldl.Fold' a b -> Parser a m b
@
-}
foldAll 
    :: Monad m
    => (x -> a -> x)
    -- ^ Step function
    -> x
    -- ^ Initial accumulator
    -> (x -> b)
    -- ^ Extraction function
    -> Parser a m b
foldAll step begin done = go begin
  where
    go x = do
        ea <- draw
        case ea of
            Nothing -> return (done x)
            Just a  -> go $! step x a
{-# INLINABLE foldAll #-}

{-| Fold all input values monadically

@
 'Control.Foldl.impurely' foldAllM :: Monad m => 'Control.Foldl.FoldM' a m b -> Parser a m b
@
-}
foldAllM
    :: Monad m
    => (x -> a -> m x)
    -- ^ Step function
    -> m x
    -- ^ Initial accumulator
    -> (x -> m b)
    -- ^ Extraction function
    -> Parser a m b
foldAllM step begin done = do
    x0 <- lift begin
    go x0
  where
    go x = do
        ea <- draw
        case ea of
            Nothing -> lift (done x)
            Just a  -> do
                x' <- lift (step x a)
                go $! x'
{-# INLINABLE foldAllM #-}

{- $parsinglenses
    Connect lenses to 'Producer's using ('Lens.Family.^.') or
    'Lens.Family.view':

> (^.) :: Producer a m x
>      -> Lens' (Producer a m x) (Producer b m y)
>      -> Producer b m y

    Connect lenses to 'Parser's using 'Lens.Family.State.Strict.zoom':

> zoom :: Lens' (Producer a m x) (Producer b m y)
>      -> Parser b m r
>      -> Parser a m r

    Connect lenses to each other using ('.') (i.e. function composition):

> (.) :: Lens' (Producer a m x) (Producer b m y)
>     -> Lens' (Producer b m y) (Producer c m z)
>     -> Lens' (Producer a m y) (Producer c m z)
-}

type Lens' a b = forall f . (Functor f) => (b -> f b) -> (a -> f a)

{-| 'span' is an improper lens from a 'Producer' to two 'Producer's split using
    the given predicate, where the outer 'Producer' is the longest consecutive
    group of elements that satisfy the predicate
-}
span
    :: Monad m
    => (a -> Bool) -> Lens' (Producer a m x) (Producer a m (Producer a m x))
span predicate k p0 = fmap join (k (to p0))
  where
--  to :: Monad m => Producer a m x -> Producer a m (Producer a m x)
    to p = do
        x <- lift (next p)
        case x of
            Left   r      -> return (return r)
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
    :: Monad m
    => Int -> Lens' (Producer a m x) (Producer a m (Producer a m x))
splitAt n0 k p0 = fmap join (k (to n0 p0))
  where
--  to :: Monad m => Int -> Producer a m x -> Producer a m (Producer a m x)
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

{-| 'groupBy' is an improper lens from a 'Producer' to two 'Producer's split
    after the first group of elements grouped using the given equality predicate
-}
groupBy
    :: Monad m
    => (a -> a -> Bool)
    -> Lens' (Producer a m x) (Producer a m (Producer a m x))
groupBy equals k p0 = fmap join (k (to p0))
  where
--  to :: Monad m => Producer a m r -> Producer a m (Producer a m x)
    to p = do
        x <- lift (next p)
        case x of
            Left   r      -> return (return r)
            Right (a, p') -> (yield a >> p') ^. span (equals a)
{-# INLINABLE groupBy #-}

-- | Like 'groupBy', where the equality predicate is ('==')
group
    :: (Monad m, Eq a) => Lens' (Producer a m x) (Producer a m (Producer a m x))
group = groupBy (==)
{-# INLINABLE group #-}

(^.) :: a -> ((b -> Constant b b) -> (a -> Constant b a)) -> b
a ^. lens = getConstant (lens Constant a)

{-| 'groupsBy' is an improper lens from a 'Producer' to a 'FreeT' of 'Producer's
    grouped using the given equality predicate
-}
groupsBy
    :: Monad m
    => (a -> a -> Bool) -> Lens' (Producer a m x) (FreeT (Producer a m) m x)
groupsBy equals k p0 = fmap concats (k (to p0))
  where
--  to :: Monad m => Producer a m r -> FreeT (Producer a m) m r
    to p = FreeT $ do
        x <- next p
        return $ case x of
            Left   r      -> Pure r
            Right (a, p') -> Free $ fmap to ((yield a >> p')^.span (equals a))
{-# INLINABLE groupsBy #-}

-- | Like 'groupsBy', where the equality predicate is ('==')
groups :: (Monad m, Eq a) => Lens' (Producer a m x) (FreeT (Producer a m) m x)
groups = groupsBy (==)
{-# INLINABLE groups #-}

{-| 'chunksOf' is an improper lens from a 'Producer' to a 'FreeT' of 'Producer's
    of fixed length
-}
chunksOf
    :: Monad m => Int -> Lens' (Producer a m x) (FreeT (Producer a m) m x)
chunksOf n0 k p0 = fmap concats (k (to p0))
  where
--  to :: Monad m => Producer a m x -> FreeT (Producer a m) m x
    to p = FreeT $ do
        x <- next p
        return $ case x of
            Left   r      -> Pure r
            Right (a, p') -> Free $ do
                p'' <- (yield a >> p')^.splitAt n0
                return (to p'')
{-# INLINABLE chunksOf #-}

-- | Join a 'FreeT'-delimited stream of 'Producer's into a single 'Producer'
concats :: Monad m => FreeT (Producer a m) m x -> Producer a m x
concats = go
  where
    go f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                go f'
{-# INLINABLE concats #-}

{-| Join a 'FreeT'-delimited stream of 'Producer's into a single 'Producer' by
    intercalating a 'Producer' in between them
-}
intercalates
    :: Monad m => Producer a m () -> FreeT (Producer a m) m x -> Producer a m x
intercalates sep = go0
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
{-# INLINABLE intercalates #-}

-- | @(takes n)@ only keeps the first @n@ functor layers of a 'FreeT'
takes :: (Functor f, Monad m) => Int -> FreeT f m () -> FreeT f m ()
takes = go
  where
    go n f =
        if (n > 0)
        then FreeT $ do
            x <- runFreeT f
            case x of
                Pure () -> return (Pure ())
                Free w  -> return (Free (fmap (go $! n - 1) w))
        else return ()
{-# INLINABLE takes #-}

{-| @(takes' n)@ only keeps the first @n@ 'Producer's of a 'FreeT'

    'takes'' differs from 'takes' by draining unused 'Producer's in order
    to preserve the return value.  This makes it a suitable argument for
    'transFreeT'
-}
takes'
    :: Monad m => Int -> FreeT (Producer a m) m x -> FreeT (Producer a m) m x
takes' = go0
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
                f' <- P.runEffect (P.for p P.discard)
                go1 f'
{-# INLINABLE takes' #-}

{-| @(drops n)@ peels off the first @n@ 'Producer' layers of a 'FreeT'

    Use carefully: the peeling off is not free.   This runs the first @n@
    layers, just discarding everything they produce.
-}
drops
    :: Monad m => Int -> FreeT (Producer a m) m x -> FreeT (Producer a m) m x
drops = go
  where
    go n ft
        | n <= 0 = ft
        | otherwise = FreeT $ do
            ff <- runFreeT ft
            case ff of
                Pure _ -> return ff
                Free f -> do
                    ft' <- P.runEffect $ P.for f P.discard
                    runFreeT $ go (n-1) ft'
{-# INLINABLE drops #-}

{-| Fold each 'Producer' of a 'FreeT'

@
 'Control.Foldl.purely' folds
     :: Monad m => 'Control.Foldl.Fold' a b -> FreeT (Producer a m) m r -> Producer b m r
@
-}
folds
    :: Monad m
    => (x -> a -> x)
    -- ^ Step function
    -> x
    -- ^ Initial accumulator
    -> (x -> b)
    -- ^ Extraction function
    -> FreeT (Producer a m) m r
    -- ^
    -> Producer b m r
folds step begin done = go
  where
    go f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
	        (f', b) <- lift (fold p begin)
	        yield b
	        go f'

    fold p x = do
        y <- next p
        case y of
            Left   f      -> return (f, done x)
            Right (a, p') -> fold p' $! step x a
{-# INLINABLE folds #-}

{-| Fold each 'Producer' of a 'FreeT', monadically

@
 'Control.Foldl.impurely' foldsM
     :: Monad m => 'Control.Foldl.FoldM' a b -> FreeT (Producer a m) m r -> Producer b m r
@
-}
foldsM
    :: Monad m
    => (x -> a -> m x)
    -- ^ Step function
    -> m x
    -- ^ Initial accumulator
    -> (x -> m b)
    -- ^ Extraction function
    -> FreeT (Producer a m) m r
    -- ^
    -> Producer b m r
foldsM step begin done = go
  where
    go f = do
        y <- lift (runFreeT f)
        case y of
            Pure r -> return r
            Free p -> do
                (f', b) <- lift $ do
                    x <- begin
		    foldM p x
                yield b
                go f'

    foldM p x = do
        y <- next p
        case y of
            Left   f      -> do
                b <- done x
                return (f, b)
            Right (a, p') -> do
                x' <- step x a
                foldM p' $! x'

{- $reexports
    "Control.Monad.Trans.Class" re-exports 'lift'.

    "Control.Monad.Trans.Free" re-exports 'FreeF', 'FreeT', 'runFreeT', and
    'transFreeT'.

    "Control.Monad.Trans.State.Strict" re-exports 'StateT', 'runStateT',
    'evalStateT', and 'execStateT'.

    "Pipes" re-exports 'Producer', 'yield', and 'next'.
-}
