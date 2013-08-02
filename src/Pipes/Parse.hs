-- | Parsing utilities for @pipes@

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

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

    -- * Isomorphisms
    -- $isomorphisms
    spans,
    splitsAt,
    groupsBy,

    -- * Utilities
    splitUsing,

    -- * Re-exports
    -- $re-exports
    module Control.Lens,
    module Control.Monad.IO.Class,
    module Control.Monad.Trans.State.Strict
    ) where

import Control.Lens (Iso', iso, zoom)
import Control.Monad (join, liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (
    StateT, runStateT, evalStateT, execStateT )
import qualified Control.Monad.Trans.State.Strict as S
import Data.Maybe (isNothing)
import Pipes (Producer, Pipe, await, yield, next, (>->))
import Pipes.Core (Producer')
import Pipes.Lift (execStateP)
import qualified Pipes.Prelude as P

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

{- $highlevel
    'input' provides a 'Producer' that streams from the underlying 'Producer'.

    Streaming from 'input' differs from streaming directly from the underlying
    'Producer' because any unused input is saved for later, as the following
    example illustrates:

> import Pipes
> import Pipes.Parse
> import qualified Pipes.Prelude as P
>
> parser1 :: (Show a) => StateT (Producer a IO r) IO ()
> parser1 = do
>     run $ for (input >-> P.take 2) (liftIO . print)
>
>     liftIO $ putStrLn "Intermission"
>
>     run $ for (input >-> P.take 2) (liftIO . print)

    The second pipeline continues where the first pipeline left off:

>>> evalStateT parser1 (each [1..])
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
    You can use 'zoom' from @Control.Lens@ to segment the underlying 'Producer'
    and limit your sub-parser to to a subset of the input stream.

    You specify the subset you are interested in using an isomorphism like
    'splitsAt' or 'spans':

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
    parser when the 'zoom' completes.

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

    Don't forget that you can nest 'zoom's or compose isomorphisms using ('.')
    to target substreams within substreams.
-}

-- 'Pipes.Prelude.takeWhile' does not 'unDraw' the element that doesn't match
takeWhile'
    :: (Monad m) => (a -> Bool) -> Pipe a a (StateT (Producer a m r) m) ()
takeWhile' predicate = loop
  where
    loop = do
        a <- await
        if (predicate a)
            then do
                yield a
                loop
            else lift (unDraw a)

{-| Isomorphism between a 'Producer' and the prefix \/ suffix generated by
    splitting the 'Producer' in a manner analogous to 'span'

    Use this isomorphism to limit a 'Producer' to the prefix of all elements
    that satisfy a given predicate.
-}
spans
    :: (Monad m)
    => (a -> Bool) -> Iso' (Producer a m r) (Producer a m (Producer a m r))
spans predicate = iso (splitUsing (takeWhile' predicate)) join
{-# INLINABLE spans #-}

{-| Isomorphism between a 'Producer' and the prefix \/ suffix generated by
    splitting the 'Producer' in a manner analogous to 'splitAt'

    Use this isomorphism to limit a 'Producer' to a fixed number of elements.
-}
splitsAt
    :: (Monad m) => Int -> Iso' (Producer a m r) (Producer a m (Producer a m r))
splitsAt n = iso (splitUsing (P.take n)) join
{-# INLINABLE splitsAt #-}

{-| Isomorphism between a 'Producer' and the prefix \/ suffix generated by
    splitting off the first group of similar elements in a manner analogous to
    'Data.List.groupBy'.

    Use this isomorphism to limit a 'Producer' only to consecutive elements that
    are equal to the first element according to the equality predicate.
-}
groupsBy
    :: (Monad m)
    => (a -> a -> Bool)
    -> Iso' (Producer a m r) (Producer a m (Producer a m r))
groupsBy equal = iso (splitUsing equals) join
  where
    equals = do
        a <- await
        yield a
        takeWhile' (equal a)
{-# INLINABLE groupsBy #-}

{-| 'splitUsing' is a utility function that library writers can use to build
    isomorphisms.

    The first argument is a 'Pipe' that delimits the input of interest, and the
    result is a function we can use for the forward direction of an
    isomorphism.  See the source code to 'splitsAt' and 'spans' for example
    usage.
-}
splitUsing
    :: (Monad m)
    => Pipe a b (StateT (Producer a m r) m) ()
    -> Producer a m r
    -> Producer b m (Producer a m r)
splitUsing p src = execStateP src (input >-> p)
{-# INLINABLE splitUsing #-}

{- $re-exports
    @Control.Lens@ re-exports 'zoom', 'Iso'', and 'iso'.

    @Control.Monad.Trans.State.Strict@ re-exports 'StateT' (the type),
    'runStateT', 'evalStateT', and 'execStateT'.

    @Control.Monad.IO.Class@ re-exports 'MonadIO'.
-}
