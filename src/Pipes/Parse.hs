{-| Element-agnostic parsing utilities for @pipes@

    One of the disadvantages of 'Pipe's is that they cannot intercept and
    handle end of input.  For example, you would not be able to write a
    'group' 'Pipe' that grouped equal elements like this:

> group :: (Monad m, Eq a) => Pipe a [a] m r

    This would not work because you would be unable to flush the final group of
    elements upon reaching end of input.

    Instead, the only way to handle these cases is to write a function from a
    'Producer' to a new 'Producer':

> group :: (Monad m, Eq a) => Producer a m () -> Producer [a] m ()

    @pipes-parse@ makes these kinds of functions easier to write:

> import Pipes
> import Pipes.Parse as Parse
> import Pipes.Prelude (toListM)
> 
> group :: (Monad m, Eq a) => Producer a m () -> Producer [a] m ()
> group p = evalStateP p loop
>   where
>     loop = do
>         ma <- lift draw
>         case ma of
>             Nothing -> return ()
>             Just a -> do
>                 as <- lift $ toListM (input >-> Parse.takeWhile (== a))
>                 yield (a:as)
>                 loop

    As a bonus, you also gain the ability to easily consume a 'Producer'
    incrementally instead of all at once, as described below.
-}

{-# LANGUAGE RankNTypes #-}

module Pipes.Parse (
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

    -- * Splitters
    groupBy,
    chunksOf,
    splitOn,

    -- * Joiners
    concat,
    intercalate,

    -- * Re-exports
    -- $re-exports
    module Control.Monad.Trans.State.Strict,
    module Pipes.Lift
    ) where

import Control.Monad (liftM, unless)
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (
    StateT, runStateT, evalStateT, execStateT )
import qualified Control.Monad.Trans.State.Strict as S
import Data.Maybe (isNothing)
import Pipes (Producer, Pipe, await, yield, next, (>->))
import Pipes.Core (Producer')
import Pipes.Lift (runStateP, evalStateP, execStateP)
import qualified Pipes.Prelude as P
import Prelude hiding (concat, takeWhile)

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
        Left   _      -> return Nothing
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

{- $isomorphisms
    You can use 'Control.Lens.zoom' from @Control.Lens@ to segment the
    underlying 'Producer' and limit your sub-parser to to a subset of the input
    stream.

    You specify the subset you want using an isomorphism like 'splitsAt' or
    'spans':

> import Control.Lens (zoom)
>
> -- A parser that prints all elements available to it
> printAll :: (Show a) => StateT (Producer a IO r) IO ()
> printAll = run $ for input (liftIO . print)
>
> parser2 :: StateT (Producer Int IO r) IO ()
> parser2 = do
>     -- Restrict the first sub-parser to elements less than 4
>     zoom (spans (< 4)) printAll
>
>     liftIO $ putStrLn "Intermission"
>
>     -- Restrict the second sub-parser to the next three elements
>     zoom (splitsAt 3) printAll

>>> evalStateT parser2 (each [1..])
1
2
3
Intermission
4
5
6

    This approach does not require you to push back any unused elements.  These
    isomorphisms correctly return unused input back to the surrounding
    parser when the 'Control.Lens.zoom' completes.

> parser3 :: StateT (Producer Int IO r) IO ()
> parser3 = do
>     -- This 'zoom' block will return back the unused 3 when it is done
>     zoom (spans (< 4)) $ run $ for (input >-> P.take 2) (liftIO . print)
>
>     liftIO $ putStrLn "Intermission"
>
>     zoom (splitsAt 3) printAll

>>> evalStateT parser3 (each [1..])
1
2
Intermission
3
4
5

    Don't forget that you can nest 'Control.Lens.zoom's or compose isomorphisms
    using ('.')
    to target substreams within substreams.
-}

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
                p2 <- liftF $ execStateP p1 $ input >-> takeWhile (equal a)
                loop p2
{-# INLINABLE groupBy #-}

{-| Split a 'Producer' into a `FreeT`-delimited stream of 'Producer's of the
    given chunk size
-}
chunksOf :: (Monad m) => Int -> Producer a m () -> FreeT (Producer a m) m () 
chunksOf n = loop
  where
    loop p = do
        (eof, p') <- liftF $ runStateP p $ do
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
        (stop, p') <- liftF $ runStateP p $ do
            input >-> takeWhile predicate
            lift $ liftM isNothing draw
        unless stop (loop p')
{-# INLINABLE splitOn #-}

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
intercalate sep = loop0
  where
    loop0 f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                loop1 f'
    loop1 f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                sep
                f' <- p
                loop1 f'
{-# INLINABLE intercalate #-}

{- $re-exports
    @Control.Monad.Trans.State.Strict@ re-exports 'StateT' (the type),
    'runStateT', 'evalStateT', and 'execStateT'.

    @Pipes.Lift@ re-exports 'runStateP', 'evalStateP', and 'execStateP'.
-}
