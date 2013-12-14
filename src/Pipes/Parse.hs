{-|
    Element-agnostic parsing utilities for @pipes@

    @pipes-parse@ provides two ways to parse and transform streams in constant
    space:

    * The \"list-like\" approach, using the split \/ transform \/ join paradigm

    * The monadic approach, using parser combinators

    The top half of this module provides the list-like approach, which is easier
    to use, but less powerful.  The key idea is that:

> -- '~' means "is analogous to"
> Producer a m ()            ~   [a]
>
> FreeT (Producer a m) m ()  ~  [[a]]

    'FreeT' nests each subsequent 'Producer' within the return value of the
    previous 'Producer' so that you cannot access the next 'Producer' until you
    completely drain the current 'Producer'.  However, you rarely need to work
    with 'FreeT' directly.  Instead, you structure everything using
    \"splitters\", \"transformations\" and \"joiners\":

> -- A "splitter"
> Producer a m ()           -> FreeT (Producer a m) m ()  ~   [a]  -> [[a]]
>
> -- A "transformation"
> FreeT (Producer a m) m () -> FreeT (Producer a m) m ()  ~  [[a]] -> [[a]]
>
> -- A "joiner"
> FreeT (Producer a m) m () -> Producer a m ()            ~  [[a]] ->  [a]

    For example, if you wanted to group standard input by equal lines and take
    the first three groups, you would write:

> import Pipes
> import qualified Pipes.Parse as Parse
> import qualified Pipes.Prelude as Prelude
>
> threeGroups :: (Monad m, Eq a) => Producer a m () -> Producer a m ()
> threeGroups = Parse.concat . Parse.takeFree 3 . Parse.groupBy (==)
> --            ^ Joiner       ^ Transformation   ^ Splitter

    This then limits standard input to the first three consecutive groups of
    equal lines:

>>> runEffect $ threeGroups Prelude.stdinLn >-> Prelude.stdoutLn
Group1<Enter>
Group1
Group1<Enter>
Group1
Group2<Enter>
Group2
Group3<Enter>
Group3
Group3<Enter>
Group3
Group4<Enter>
>>> -- Done, because we began entering our fourth group

    The advantage of this style or programming is that you never bring more than
    a single element into memory.  This works because `FreeT` sub-divides the
    `Producer` without concatenating elements together, preserving the laziness
    of the underlying 'Producer'.

    The bottom half of this module lets you implement your own list-like
    transformations using monadic parsers.

    For example, if you wanted to repeatedly sum every 3 elements and yield the
    result, you would write:

> import Control.Monad (unless)
> import Pipes
> import qualified Pipes.Prelude as P
> import Pipes.Parse
>
> sum3 :: (Monad m, Num a) => Producer a (StateT (Producer a m ()) m) ()
> sum3 = do
>     eof <- lift isEndOfInput
>     unless eof $ do
>         n <- lift $ P.sum (input >-> P.take 3)
>         yield n
>         sum3

    When you are done building the parser, you convert your parser to a
    list-like function using `evalStateP`:

> import Pipes.Lift (evalStateP)
>
> -- sum3'  ~  (Num a) => [a] -> [a]
>
> sum3' :: (Monad m, Num a) => Producer a m () -> Producer a m ()
> sum3' p = evalStateP p sum3

    ... then apply it to the `Producer` you want to transform:

>>> runEffect $ sum3' (P.readLn >-> P.takeWhile (/= 0)) >-> P.print
1<Enter>
4<Enter>
5<Enter>
10
2<Enter>
0<Enter>
2
>>>

-}

{-# LANGUAGE RankNTypes #-}

module Pipes.Parse (
    -- * Isomorphisms
    span,
    splitAt,
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

    -- * Parsers
    -- $lowlevel
    draw,
    drawAll,
    unDraw,
    peek,
    isEndOfInput,

    -- * Re-exports
    -- $reexports
    module Lens.Family,
    module Lens.Family.State.Strict,
    module Control.Monad.Trans.Free,
    module Control.Monad.Trans.State.Strict
    ) where

import Control.Monad (join)
import Control.Monad.Trans.Free (
    FreeF(Pure, Free), FreeT(FreeT, runFreeT), transFreeT )
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.State.Strict (
    StateT(StateT, runStateT), evalStateT, execStateT )
import Lens.Family ((^.), over)
import Lens.Family.State.Strict (zoom)
import Pipes
import Data.Profunctor (Profunctor, dimap)
import Prelude hiding (concat, takeWhile, splitAt, span)

{-| 'span' is an improper isomorphism that splits a 'Producer' in two using
    the predicate in the forward direction and 'join's the two 'Producer's in
    the other direction.

> span
>     :: (Monad m)
>     => (a -> Bool) -> Iso' (Producer a m r) (Producer a m (Producer a m r))
-}
span
    :: (Functor f, Monad m, Profunctor p)
    => (a -> Bool)
    -> p (Producer a m (Producer a m r)) (f (Producer a m (Producer a m r)))
    -- ^
    -> p (Producer a m               r ) (f (Producer a m               r ))
    -- ^
span predicate = dimap to (fmap join)
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

{-| 'splitAt' is an improper isomorphism that splits a 'Producer' in two after
    the given number of elements in the forward direction and 'join's the two
    'Producer's in the other direction.

> splitAt
>     :: (Monad m)
>     => Int -> Iso' (Producer a m r) (Producer a m (Producer a m r))
-}
splitAt
    :: (Functor f, Monad m, Profunctor p)
    => Int
    -> p (Producer a m (Producer a m r)) (f (Producer a m (Producer a m r)))
    -- ^
    -> p (Producer a m               r ) (f (Producer a m               r ))
    -- ^
splitAt n0 = dimap (to n0) (fmap join)
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

{-| 'groupBy' is an improper isomorphism that groups a `Producer` by the
    supplied equality predicate in the forward direction and concatenates the
    groups in the reverse direction.

> groupBy
>     :: (Monad m)
>     => (a -> a -> Bool) -> Iso' (Producer a m r) (FreeT (Producer a m) m r)
-}
groupBy
    :: (Functor f, Monad m, Profunctor p)
    => (a -> a -> Bool)
    -- ^
    -> p (FreeT (Producer a m) m r) (f (FreeT (Producer a m) m r))
    -- ^
    -> p (Producer  a  m r) (f (Producer  a  m r))
groupBy equals = dimap to (fmap concat)
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

{-| Like 'groupBy', where the equality predicate is ('==')

> group
>     :: (Monad m, Eq a)
>     => Iso' (Producer a m r) (FreeT (Producer a m) m r)
-}
group
    :: (Functor f, Monad m, Eq a, Profunctor p)
    => p (FreeT (Producer a m) m r) (f (FreeT (Producer a m) m r))
    -- ^
    -> p (Producer  a  m r) (f (Producer  a  m r))
group = groupBy (==)
{-# INLINABLE group #-}

{-| 'chunksOf' is an improper isomorphism that groups a `Producer` into
    sub-'Producer's of fixed size in the forward direction and concatenates the
    groups in the reverse direction.

> chunksOf
>     :: (Monad m)
>     => Int -> Iso' (Producer a m r) (FreeT (Producer a m) m r)
-}
chunksOf
    :: (Functor f, Monad m, Profunctor p)
    => Int
    -> p (FreeT (Producer a m) m r) (f (FreeT (Producer a m) m r))
    -- ^
    -> p (Producer a m r) (f (Producer a m r))
    -- ^
chunksOf n0 = dimap to (fmap concat)
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
concat :: (Monad m) => FreeT (Producer a m) m r -> Producer a m r
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
    => Producer a m () -> FreeT (Producer a m) m r -> Producer a m r
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
    :: (Monad m) => Int -> FreeT (Producer a m) m r -> FreeT (Producer a m) m r
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
    :: (Monad m) => Int -> FreeT (Producer a m) m r -> FreeT (Producer a m) m r
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

{- $lowlevel
    @pipes-parse@ handles end-of-input and pushback by storing a 'Producer' in
    a 'StateT' layer.
-}

{-| Draw one element from the underlying 'Producer', returning 'Left' if the
    'Producer' is empty
-}
draw :: (Monad m) => StateT (Producer a m r) m (Either r a)
draw = do
    p <- S.get
    x <- lift (next p)
    case x of
        Left   r      -> do
            S.put (return r)
            return (Left r)
        Right (a, p') -> do
            S.put p'
            return (Right a)
{-# INLINABLE draw #-}

{-| Draw all elements from the underlying 'Producer'

    Note that 'drawAll' is not an idiomatic use of @pipes-parse@, but I provide
    it for simple testing purposes.  Idiomatic @pipes-parse@ style consumes the
    elements immediately as they are generated instead of loading all elements
    into memory.
-}
drawAll :: (Monad m) => StateT (Producer a m r) m [a]
drawAll = go id
  where
    go diffAs = do
        ma <- draw
        case ma of
            Left  _ -> return (diffAs [])
            Right a -> go (diffAs . (a:))
{-# INLINABLE drawAll #-}

-- | Push back an element onto the underlying 'Producer'
unDraw :: (Monad m) => a -> StateT (Producer a m r) m ()
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
peek :: (Monad m) => StateT (Producer a m r) m (Either r a)
peek = do
    x <- draw
    case x of
        Left  _ -> return ()
        Right a -> unDraw a
    return x
{-# INLINABLE peek #-}

{-| Check if the underlying 'Producer' is empty

> isEndOfInput = liftM isLeft peek
-}
isEndOfInput :: (Monad m) => StateT (Producer a m r) m Bool
isEndOfInput = do
    x <- peek
    return (case x of
        Left  _ -> True
        Right _ -> False )
{-# INLINABLE isEndOfInput #-}

{- $reexports
    @Lens.Family@ re-exports ('^.') and 'over'.

    @Lens.Family.State.Strict@ re-exports 'zoom'.

    @Control.Monad.Trans.Free@ re-exports 'FreeF', 'FreeT', 'runFreeT', and
    'transFreeT'.

    @Control.Monad.Trans.State.Strict@ re-exports 'StateT', 'runStateT',
    'evalStateT', and 'execStateT'.
-}
