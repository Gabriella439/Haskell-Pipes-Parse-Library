{-|
    Element-agnostic parsing utilities for @pipes@

    @pipes-parse@ provides two ways to parse and transform streams in constant
    space:

    * The \"list-like\" approach, using the split \/ transform \/ join paradigm

    * The monadic approach, using parser combinators

    The top half of this module provides the list-like approach.  The key idea
    is that:

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

    The bottom half of this module contains the lower-level monadic parsing
    primitives.  These are more useful for `pipes` implementers, particularly
    for building splitters.  I recommend that application developers use the
    list-like style whenever possible.
-}

{-# LANGUAGE RankNTypes #-}

module Pipes.Parse (
    -- * Splitters
    groupBy,
    chunksOf,
    splitOn,

    -- * Transformations
    takeFree,

    -- * Joiners
    concat,
    intercalate,

    -- * Low-level Parsers
    -- $lowlevel
    draw,
    unDraw,
    peek,
    isEndOfInput,

    -- * High-level Parsers
    -- $highlevel
    input,

    -- * Utilities
    takeWhile,

    -- * Re-exports
    -- $reexports
    module Control.Monad.Trans.Free,
    module Control.Monad.Trans.State.Strict
    ) where

import Control.Applicative ((<$>), (<$))
import qualified Control.Monad.Trans.Free as F
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free (FreeF(Pure, Free), FreeT(FreeT, runFreeT))
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.State.Strict (
    StateT(StateT, runStateT), evalStateT, execStateT )
import Pipes (Producer, Pipe, await, yield, next, (>->), Producer')
import Pipes.Lift (runStateP)
import qualified Pipes.Prelude as P
import Prelude hiding (concat, takeWhile)

{-| Split a 'Producer' into a `FreeT`-delimited stream of 'Producer's grouped by
    the supplied equality predicate
-}
groupBy
    :: (Monad m)
    => (a -> a -> Bool) -> Producer a m r -> FreeT (Producer a m) m r
groupBy equal = loop
  where
    loop p = do
        (x, p') <- F.liftF $ runStateP p $ do
            x <- lift draw
            case x of
                Left  r -> return (Just r)
                Right a -> do
                    yield a
                    (Just <$> input) >-> (Nothing <$ takeWhile (equal a))
        case x of
            Just r  -> return r
            Nothing -> loop p'
{-# INLINABLE groupBy #-}

{-| Split a 'Producer' into a `FreeT`-delimited stream of 'Producer's of the
    given chunk size
-}
chunksOf :: (Monad m) => Int -> Producer a m r -> FreeT (Producer a m) m r
chunksOf n = loop
  where
    loop p = do
        (x, p') <- F.liftF $ runStateP p $
            (Just <$> input) >-> (Nothing <$ P.take n)
        case x of
            Just r  -> return r
            Nothing -> loop p'
{-# INLINABLE chunksOf #-}

{-| Split a 'Producer' into a `FreeT`-delimited stream of 'Producer's separated
    by elements that satisfy the given predicate
-}
splitOn
    :: (Monad m) => (a -> Bool) -> Producer a m r -> FreeT (Producer a m) m r
splitOn predicate = loop
  where
    loop p = do
        (x, p') <- F.liftF $ runStateP p $
            (Just <$> input) >-> (Nothing <$ takeWhile (not . predicate))
        case x of
            Just r  -> return r
            Nothing -> loop p'
{-# INLINABLE splitOn #-}

-- | Join a 'FreeT'-delimited stream of 'Producer's into a single 'Producer'
concat :: (Monad m) => FreeT (Producer a m) m r -> Producer a m r
concat = loop
  where
    loop f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                concat f'
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

-- | @(take n)@ only keeps the first @n@ functor layers of a 'FreeT'
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

{- $highlevel
    'input' provides a 'Producer' that streams from the underlying 'Producer'.

    Streaming from 'input' differs from streaming directly from the underlying
    'Producer' because any unused input is saved for later, as the following
    example illustrates:

> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.State.Strict
> import Pipes
> import Pipes.Parse
> import qualified Pipes.Prelude as P
>
> parser :: (Show a) => StateT (Producer a IO r) IO ()
> parser = do
>     runEffect $ input >-> P.take 2 >-> P.show >-> hoist liftIO P.stdoutLn
>
>     liftIO $ putStrLn "Intermission"
>
>     runEffect $ input >-> P.take 2 >-> P.show >-> hoist liftIO P.stdoutLn

    The second pipeline resumes where the first pipeline left off:

>>> evalStateT parser (each [1..])
1
2
Intermission
3
4

    You can see more examples of how to use these parsing utilities by studying
    the source code for the above splitters.
-}

-- | Stream from the underlying 'Producer'
input :: (Monad m) => Producer' a (StateT (Producer a m r) m) r
input = loop
  where
    loop = do
        x <- lift draw
        case x of
            Left  r -> return r
            Right a -> do
                yield a
                loop
{-# INLINABLE input #-}

{-| A variation on 'Pipes.Prelude.takeWhile' from @Pipes.Prelude@ that 'unDraw's
    the first element that does not match
-}
takeWhile
    :: (Monad m) => (a -> Bool) -> Pipe a a (StateT (Producer a m r) m) ()
takeWhile predicate = loop
  where
    loop = do
        a <- await
        if (predicate a)
            then do
                yield a
                loop
            else lift (unDraw a)
{-# INLINABLE takeWhile #-}

{- $reexports
    @Control.Monad.Trans.Free@ re-exports 'FreeF', 'FreeT', and 'runFreeT'.

    @Control.Monad.Trans.State.Strict@ re-exports 'StateT', 'runStateT',
    'evalStateT', and 'execStateT'.
-}
