-- | Element-agnostic parsing utilities for @pipes@

{-# LANGUAGE RankNTypes #-}

module Pipes.Parse (
    -- * Splitters
    -- $splitters
    groupBy,
    chunksOf,
    splitOn,

    -- * Joiners
    -- $joiners
    concat,
    intercalate,

    -- * Low-level Interface
    -- $lowlevel
    draw,
    unDraw,
    peek,
    isEndOfInput,

    -- * High-level Interface
    -- $highlevel
    input,

    -- * Utilities
    takeWhile,

    -- * Re-exports
    -- $reexports
    module Control.Monad.Trans.Free,
    module Control.Monad.Trans.State.Strict,
    module Pipes.Lift
    ) where

import Control.Monad (liftM, unless)
import qualified Control.Monad.Trans.Free as F
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free (FreeF(Pure, Free), FreeT(FreeT, runFreeT))
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.State.Strict (
    StateT(StateT, runStateT), evalStateT, execStateT )
import Data.Maybe (isNothing)
import Pipes (Producer, Pipe, await, yield, next, (>->))
import Pipes.Core (Producer')
import Pipes.Lift (runStateP, evalStateP, execStateP)
import qualified Pipes.Prelude as P
import Prelude hiding (concat, takeWhile)

{- $splitters
    @pipes-parse@ uses 'FreeT' to sub-divide streams into groups without
    collecting elements in memory.

    Think of @(Producer a m ())@ as being analogous to @[a]@ and
    @(FreeT (Producer a m) ())@ as being analogous to @[[a]]@.  'FreeT' in this
    case behaves like a linked list of 'Producer's where you must drain each
    'Producer' to completion before you can advance to the next 'Producer'.
-}

{-| Split a 'Producer' into a `FreeT`-delimited stream of 'Producer's grouped by
    the supplied equality predicate
-}
groupBy
    :: (Monad m)
    => (a -> a -> Bool) -> Producer a m () -> FreeT (Producer a m) m ()
groupBy equal = loop
  where
    loop p = do
        x <- lift (next p)
        case x of
            Left  r       -> return r
            Right (a, p1) -> do
                p2 <- F.liftF $ execStateP p1 $ do
                    yield a
                    input >-> takeWhile (equal a)
                loop p2
{-# INLINABLE groupBy #-}

{-| Split a 'Producer' into a `FreeT`-delimited stream of 'Producer's of the
    given chunk size
-}
chunksOf :: (Monad m) => Int -> Producer a m () -> FreeT (Producer a m) m () 
chunksOf n = loop
  where
    loop p = do
        (eof, p') <- F.liftF $ runStateP p $ do
            input >-> P.take n
            lift isEndOfInput
        unless eof (loop p')
{-# INLINABLE chunksOf #-}

{-| Split a 'Producer' into a `FreeT`-delimited stream of 'Producer's separated
    by elements that satisfy the given predicate
-}
splitOn
    :: (Monad m) => (a -> Bool) -> Producer a m () -> FreeT (Producer a m) m ()
splitOn predicate = loop
  where
    loop p = do
        (stop, p') <- F.liftF $ runStateP p $ do
            input >-> takeWhile (not . predicate)
            lift $ liftM isNothing draw
        unless stop (loop p')
{-# INLINABLE splitOn #-}

{- $joiners
    Use the following joining functions to collect 'FreeT'-delimited streams of
    'Producer's back into a single 'Producer' while still preserving streaming.
-}

-- | Join a 'FreeT'-delimited stream of 'Producer's into a single 'Producer'
concat :: (Monad m) => FreeT (Producer a m) m () -> Producer a m ()
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
    => Producer a m () -> FreeT (Producer a m) m () -> Producer a m ()
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

{- $lowlevel
    @pipes-parse@ handles end-of-input and pushback by storing a 'Producer' in
    a 'StateT' layer.
-}

{-| Draw one element from the underlying 'Producer', returning 'Nothing' if the
    'Producer' is empty
-}
draw :: (Monad m) => StateT (Producer a m r) m (Maybe a)
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

-- | Push back an element onto the underlying 'Producer'
unDraw :: (Monad m) => a -> StateT (Producer a m r) m ()
unDraw a = S.modify (yield a >>)
{-# INLINABLE unDraw #-}

{-| 'peek' checks the first element of the stream, but uses 'unDraw' to push the
    element back so that it is available for the next 'draw' command.

> peek = do
>     ma <- draw
>     case ma of
>         Nothing -> return ()
>         Just a  -> unDraw a
>     return ma
-}
peek :: (Monad m) => StateT (Producer a m r) m (Maybe a)
peek = do
    ma <- draw
    case ma of
        Nothing -> return ()
        Just a  -> unDraw a
    return ma
{-# INLINABLE peek #-}

{-| Check if the underlying 'Producer' is empty

> isEndOfInput = liftM isNothing peek
-}
isEndOfInput :: (Monad m) => StateT (Producer a m r) m Bool
isEndOfInput = liftM isNothing peek
{-# INLINABLE isEndOfInput #-}

{- $highlevel
    'input' provides a 'Producer' that streams from the underlying 'Producer'.

    Streaming from 'input' differs from streaming directly from the underlying
    'Producer' because any unused input is saved for later, as the following
    example illustrates:

> import Control.Monad.IO.Class (liftIO)
> import Pipes
> import Pipes.Parse
> import qualified Pipes.Prelude as P
>
> parser :: (Show a) => StateT (Producer a IO r) IO ()
> parser = do
>     run $ input >-> P.take 2 >-> P.show >-> hoist liftIO P.stdout
>
>     liftIO $ putStrLn "Intermission"
>
>     run $ input >-> P.take 2 >-> P.show >-> hoist liftIO P.stdout

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
input :: (Monad m) => Producer' a (StateT (Producer a m r) m) ()
input = loop
  where
    loop = do
        ma <- lift draw
        case ma of
            Nothing -> return ()
            Just a  -> do
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

    @Pipes.Lift@ re-exports 'runStateP', 'evalStateP', and 'execStateP'.
-}
