{-| Element-agnostic parsing utilities for @pipes@

    See "Pipes.Parse.Tutorial" for an extended tutorial
-}

{-# LANGUAGE RankNTypes #-}

module Pipes.Parse (
    -- * Parsing
    -- $parsing
      Parser
    , draw
    , skip
    , drawAll
    , skipAll
    , unDraw
    , peek
    , isEndOfInput
    , foldAll
    , foldAllM

    -- * Parsing Lenses
    -- $parsinglenses
    , span
    , splitAt
    , groupBy
    , group

    -- * Utilities
    , toParser
    , toParser_
    , parsed
    , parsed_
    , parseForever
    , parseForever_

    -- * Re-exports
    -- $reexports
    , module Control.Monad.Trans.Class
    , module Control.Monad.Trans.State.Strict
    , module Pipes
    ) where

import Control.Monad (join, forever, liftM)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.State.Strict (
    StateT(StateT, runStateT), evalStateT, execStateT )
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Foldable (forM_)
import Pipes.Internal (unsafeHoist, closed)
import Pipes (Producer, yield, next)
import Pipes as NoReexport

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
    into memory.  For example, you can use 'foldAll' or 'foldAllM' for this
    purpose.
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
    forM_ x unDraw
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

> Control.Foldl.purely foldAll :: Monad m => Fold a b -> Parser a m b
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

> Control.Foldl.impurely foldAllM :: Monad m => FoldM a m b -> Parser a m b
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

type Lens' a b = forall f . (Functor f) => (b -> f b) -> a -> f a

{-| 'span' is an improper lens that splits the 'Producer' into two 'Producer's,
    where the outer 'Producer' is the longest consecutive group of elements that
    satisfy the predicate
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
                if predicate a
                then do
                    yield a
                    to p'
                else return (yield a >> p')
{-# INLINABLE span #-}

{-| 'splitAt' is an improper lens that splits a 'Producer' into two 'Producer's
    after a fixed number of elements
-}
splitAt
    :: Monad m
    => Int -> Lens' (Producer a m x) (Producer a m (Producer a m x))
splitAt n0 k p0 = fmap join (k (to n0 p0))
  where
--  to :: Monad m => Int -> Producer a m x -> Producer a m (Producer a m x)
    to n p =
        if n <= 0
        then return p
        else do
            x <- lift (next p)
            case x of
                Left   r      -> return (return r)
                Right (a, p') -> do
                    yield a
                    to (n - 1) p'
{-# INLINABLE splitAt #-}

(^.) :: a -> ((b -> Constant b b) -> a -> Constant b a) -> b
a ^. lens = getConstant (lens Constant a)

{-| 'groupBy' splits a 'Producer' into two 'Producer's after the first group of
     elements that are equal according to the equality predicate
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

{-| Convert a 'Consumer' to a 'Parser'

    'Nothing' signifies end of input
-}
toParser :: Monad m => Consumer (Maybe a) m r -> Parser a m r
toParser consumer = runEffect (lift draw >~ unsafeHoist lift consumer)
{-# INLINABLE toParser #-}

-- | Convert a never-ending 'Consumer' to a 'Parser'
toParser_ :: Monad m => Consumer a m X -> Parser a m ()
toParser_ consumer = StateT $ \producer -> do
    r <- runEffect (producer >-> fmap closed consumer)
    return ((), return r)
{-# INLINABLE toParser_ #-}


{-| Run a `Parser` repeatedly on a `Producer`, `yield`ing each `Right result

    Returns the remainder of the `Producer` when the `Parser` returns `Left`
-}
parsed
    :: Monad m
    => Parser a m (Either e b)
    -> Producer a m r -> Producer b m (e, Producer a m r)
parsed parser = go
  where
    go p = do
        (x, p') <- lift (runStateT parser p)
        case x of
            Left  r -> return (r, p')
            Right b -> do
                yield b
                go p'
{-# INLINABLE parsed #-}

{-| Run a `Parser` repeatedly on a `Producer`, `yield`ing each `Just` result

    Returns the remainder of the `Producer` when the `Parser` returns `Just`
-}
parsed_
    :: Monad m
    => Parser a m (Maybe b)
    -> Producer a m r
    -> Producer b m (Producer a m r)
parsed_ parser p = do
    ((), p') <- parsed parser' p
    return p'
  where
    parser' = do
        x <- parser
        return (case x of
            Nothing -> Left ()
            Just b  -> Right b )
{-# INLINABLE parsed_ #-}

-- | Convert a 'Parser' to a 'Pipe' by running it repeatedly on the input
parseForever ::
  Monad m =>
  (forall n. Monad n => Parser a n (Either r b)) ->
  Pipe a b m r
parseForever parse = go (forever (lift await >>= yield))
  where go prod = do (b, prod') <- runStateT parse prod
                     either return ((>> go prod') . yield) b
{-# DEPRECATED parseForever "Use `parsed` instead" #-}

-- | Variant of `parseForever` for parsers which return a Maybe
-- instead of an Either
parseForever_ ::
  Monad m =>
  (forall n. Monad n => Parser a n (Maybe b)) ->
  Pipe a b m ()
parseForever_ parse = parseForever (liftM (maybe (Left ()) Right) parse)
{-# DEPRECATED parseForever_ "Use `parsed_` instead" #-}

{- $reexports
    "Control.Monad.Trans.Class" re-exports 'lift'.

    "Control.Monad.Trans.State.Strict" re-exports 'StateT', 'runStateT',
    'evalStateT', and 'execStateT'.

    "Pipes" re-exports 'Producer', 'yield', and 'next'.
-}
