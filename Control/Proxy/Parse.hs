{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides utilities for handling end of input and push-back
module Control.Proxy.Parse (
    -- * Running parsers
    BufferT,
    runBufferT,
    parseWith,

    -- * Primitive parsers
    drawMay,
    unDraw,

    -- * Parsers that cannot fail
    drawWhen,
    skipWhen,
    drawUpToN,
    skipUpToN,
    passUpToN,
    drawWhile,
    skipWhile,
    passWhile,
    drawAll,
    skipAll,
    passAll,
    isEndOfInput,
    peek,

    -- * Parsers that can fail
    draw,
    skip,
    drawIf,
    skipIf,
    drawN,
    skipN,
    passN,
    endOfInput,

    -- * Error messages
    parseFail,
    (<??>),

    -- * Parse exception
    ParseFailure(..),

    -- * Re-exports
    -- $reexport
    module Control.Proxy.Trans.Either
    ) where

import Control.Exception (SomeException, Exception, toException, fromException)
import Control.Monad.Trans.Class (lift)
import qualified Control.Proxy as P
import Control.Proxy ((>>~), (//>), (>\\), (>->))
import Control.Proxy.Parse.Internal (BufferT, runBufferT, get, put)
import qualified Control.Proxy.Trans.Either as E
import Control.Proxy.Trans.Either (EitherP, runEitherP, runEitherK)
import qualified Control.Proxy.Trans.State as S
import Data.Typeable (Typeable)

{-| @(source \`parseWith\` parser)@ uses @(parser)@ to parse the given
    @(source)@, storing all leftovers in the provided 'BufferT' layer.
-}
parseWith
    :: (Monad m, P.Proxy p)
    => (b'  -> p a'        a  b' b (BufferT i m) r)
    -> (c'_ -> p b' (Maybe b) c' c (BufferT i m) r)
    -> (c'_ -> p a'        a  c' c (BufferT i m) r)
parseWith source parser = onlyK source >-> parser'
  where
    wrap a = do
        a' <- P.respond (Just a)
        a2 <- P.request a'
        wrap a2
    onlyP p = P.runIdentityP (do
        r <- P.IdentityP p >>~ wrap
        P.respond Nothing
        return r )
    onlyK k q = onlyP (k q)
    parser' q = P.runIdentityP $ do
        lift $ put []
        P.IdentityP $ parser q

-- | Request 'Just' one element or 'Nothing' if at end of input
drawMay :: (Monad m, P.Proxy p) => P.Pipe p (Maybe a) b (BufferT a m) (Maybe a)
drawMay = P.runIdentityP (do
    s <- lift get
    case s of
        []     -> do
            ma <- P.request ()
            case ma of
                Nothing -> lift (put [ma])
                _       -> return ()
            return ma
        ma:mas -> do
            case ma of
                Nothing -> return ()
                Just a  -> lift (put mas)
            return ma )
{-# INLINE drawMay #-}

-- | Push a single element into the leftover buffer
unDraw :: (Monad m, P.Proxy p) => a -> P.Pipe p (Maybe a) b (BufferT a m) ()
unDraw a = P.runIdentityP (lift (do
    s <- get
    put (Just a:s) ))
{-# INLINABLE unDraw #-}

-- | Draw an element only when it satisfies the predicate
drawWhen
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe p (Maybe a) b (BufferT a m) (Maybe a)
drawWhen pred = P.runIdentityP $ do
    ma <- drawMay
    case ma of
        Nothing -> return ma
        Just a  -> if (pred a)
            then return ma
            else do
                unDraw a
                return Nothing
{-# INLINABLE drawWhen #-}

-- | Skip an element only when it satisfies the predicate
skipWhen
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe p (Maybe a) b (BufferT a m) ()
skipWhen pred = P.runIdentityP $ do
    ma <- drawMay
    case ma of
        Nothing -> return ()
        Just a  -> if (pred a)
            then return ()
            else unDraw a
{-# INLINABLE skipWhen #-}

-- | Draw up to the specified number of elements
drawUpToN
    :: (Monad m, P.Proxy p) => Int -> P.Pipe p (Maybe a) b (BufferT a m) [a]
drawUpToN n0 = P.runIdentityP (go id n0)
  where
    go diffAs n = if (n > 0)
        then do
            ma <- drawMay
            case ma of
                Nothing -> return (diffAs [])
                Just a  -> go (diffAs . (a:)) $! n - 1
        else return (diffAs [])
{-# INLINABLE drawUpToN #-}

{-| Skip up to the specified number of elements

    Faster than 'drawUpToN' if you don't need the input
-}
skipUpToN
    :: (Monad m, P.Proxy p) => Int -> P.Pipe p (Maybe a) b (BufferT a m) ()
skipUpToN n0 = P.runIdentityP (go n0)
  where
    go n = if (n > 0)
        then do
            ma <- drawMay
            case ma of
                Nothing -> return ()
                Just _  -> go $! n - 1
        else return ()
{-# INLINABLE skipUpToN #-}

{-| Pass up to the specified number of elements

    'passUpToN' is a Kleisli arrow suitable as a source for 'parseWith'
-}
passUpToN
    :: (Monad m, P.Proxy p)
    => Int -> () -> P.Pipe p (Maybe a) a (BufferT a m) ()
passUpToN n0 () = P.runIdentityP (go n0)
  where
    go n = if (n > 0)
        then do
            ma <- drawMay
            case ma of
                Nothing -> return ()
                Just a  -> do
                    P.respond a
                    go $! n - 1
        else return ()
{-# INLINABLE passUpToN #-}

-- | Request as many consecutive elements satisfying a predicate as possible
drawWhile
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe p (Maybe a) b (BufferT a m) [a]
drawWhile pred = P.runIdentityP (go id)
  where
    go diffAs = do
        ma <- drawMay
        case ma of
            Nothing -> return (diffAs [])
            Just a  -> if (pred a)
                then go (diffAs . (a:))
                else do
                    unDraw a
                    return (diffAs [])
{-# INLINABLE drawWhile #-}

{-| Request as many consecutive elements satisfying a predicate as possible

    Faster than 'drawWhile' if you don't need the input
-}
skipWhile
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe p (Maybe a) b (BufferT a m) ()
skipWhile pred = P.runIdentityP go
  where
    go = do
        ma <- drawMay
        case ma of
            Nothing -> return ()
            Just a  -> if (pred a)
                then go
                else unDraw a
{-# INLINABLE skipWhile #-}

{-| Pass as many consecutive elements satisfying a predicate as possible

    'passWhile' is a Kleisli arrow suitable as a source for 'parseWith'
-}
passWhile
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> () -> P.Pipe p (Maybe a) a (BufferT a m) ()
passWhile pred () = P.runIdentityP go
  where
    go = do
        ma <- drawMay
        case ma of
            Nothing -> return ()
            Just a  -> if (pred a)
                then do
                    P.respond a
                    go
                else unDraw a
{-# INLINABLE passWhile #-}

-- | Request the rest of the input
drawAll :: (Monad m, P.Proxy p) => P.Pipe p (Maybe a) b (BufferT a m) [a]
drawAll = P.runIdentityP (go id)
  where
    go diffAs = do
        ma <- drawMay
        case ma of
            Nothing -> return (diffAs [])
            Just a  -> go (diffAs . (a:))
{-# INLINABLE drawAll #-}

{-| Skip the rest of the input

    Faster than 'drawAll' if you don't need the input
-}
skipAll :: (Monad m, P.Proxy p) => P.Pipe p (Maybe a) b (BufferT a m) ()
skipAll = P.runIdentityP go
  where
    go = do
        ma <- drawMay
        case ma of
            Nothing -> return ()
            Just _  -> go
{-# INLINABLE skipAll #-}

{-| Pass the rest of the input

    'passAll' is a Kleisli arrow suitable as a source for 'parseWith'
-}
passAll :: (Monad m, P.Proxy p) => () -> P.Pipe p (Maybe a) a (BufferT a m) ()
passAll () = P.runIdentityP go
  where
    go = do
        ma <- drawMay
        case ma of
            Nothing -> return ()
            Just a  -> do
                P.respond a
                go
{-# INLINABLE passAll #-}

-- | Return whether cursor is at end of input
isEndOfInput :: (Monad m, P.Proxy p) => P.Pipe p (Maybe a) b (BufferT a m) Bool
isEndOfInput = P.runIdentityP $ do
    ma <- peek
    return (case ma of
        Nothing -> True
        _       -> False )
{-# INLINABLE isEndOfInput #-}

-- | Look ahead one element without consuming it
peek :: (Monad m, P.Proxy p) => P.Pipe p (Maybe a) b (BufferT a m) (Maybe a)
peek = P.runIdentityP $ do
    ma <- drawMay
    case ma of
        Nothing -> return ()
        Just a  -> unDraw a
    return ma
{-# INLINABLE peek #-}

-- | Request a single element
draw
    :: (Monad m, P.Proxy p)
    => P.Pipe (E.EitherP SomeException p) (Maybe a) b (BufferT a m) a
draw = do
    ma <- drawMay
    case ma of
        Nothing -> parseFail "draw: End of input"
        Just a  -> return a
{-# INLINABLE draw #-}

-- | Skip a single element
skip
    :: (Monad m, P.Proxy p)
    => P.Pipe (E.EitherP SomeException p) (Maybe a) b (BufferT a m) ()
skip = do
    ma <- drawMay
    case ma of
        Nothing -> parseFail "skip: End of input"
        Just _  -> return ()
{-# INLINABLE skip #-}

-- | Request a single element that must satisfy the predicate
drawIf
    :: (Monad m, P.Proxy p)
    => (a -> Bool)
    -> P.Pipe (E.EitherP SomeException p) (Maybe a) b (BufferT a m) a
drawIf pred = do
    ma <- drawMay
    case ma of
        Nothing -> parseFail "drawIf: End of input"
        Just a  -> if (pred a)
            then return a
            else parseFail "drawIf: Element failed predicate"
{-# INLINABLE drawIf #-}

-- | Skip a single element that must satisfy the predicate
skipIf
    :: (Monad m, P.Proxy p)
    => (a -> Bool)
    -> P.Pipe (E.EitherP SomeException p) (Maybe a) b (BufferT a m) ()
skipIf pred = do
    ma <- drawMay
    case ma of
        Nothing -> parseFail "skipIf: End of input"
        Just a  -> if (pred a)
            then return ()
            else parseFail "skipIf: Elemented failed predicate"
{-# INLINABLE skipIf #-}

-- | Request a fixed number of elements
drawN
    :: (Monad m, P.Proxy p)
    => Int
    -> P.Pipe (E.EitherP SomeException p) (Maybe a) b (BufferT a m) [a]
drawN n0 = go id n0 where
    go diffAs n = if (n > 0)
        then do
            ma <- drawMay
            case ma of
                Nothing -> parseFail (
                    "drawN " ++ show n0 ++ ": Found only " ++ show (n0 - n)
                 ++ " elements" )
                Just a  -> go (diffAs . (a:)) $! n - 1
        else return (diffAs [])
{-# INLINABLE drawN #-}

{-| Skip a fixed number of elements

    Faster than 'drawN' if you don't need the input
-}
skipN
    :: (Monad m, P.Proxy p)
    => Int -> P.Pipe (E.EitherP SomeException p) (Maybe a) b (BufferT a m) ()
skipN n0 = go n0
  where
    go n = if (n > 0)
        then do
            ma <- drawMay
            case ma of
                Nothing -> parseFail (
                    "skipN " ++ show n0 ++ ": Found only " ++ show (n0 - n)
                 ++ " elements" )
                Just _  -> go $! n - 1
        else return ()
{-# INLINABLE skipN #-}

{-| Pass a fixed number of elements

    'passN' is a Kleisli arrow suitable as a source for 'parseWith'
-}
passN
    :: (Monad m, P.Proxy p)
    => Int
    -> () -> P.Pipe (E.EitherP SomeException p) (Maybe a) a (BufferT a m) ()
passN n0 () = go n0
  where
    go n = if (n > 0)
        then do
            ma <- drawMay
            case ma of
                Nothing -> parseFail (
                    "passN " ++ show n0 ++ ": Found only " ++ show (n0 - n)
                 ++ " elements" )
                Just _  -> go $! n - 1
        else return ()
{-# INLINABLE passN #-}

-- | Match end of input without consuming it
endOfInput
    :: (Monad m, P.Proxy p)
    => P.Pipe (E.EitherP SomeException p) (Maybe a) b (BufferT a m) ()
endOfInput = do
    b <- isEndOfInput
    if b then return () else parseFail "endOfInput: Not end of input"
{-# INLINABLE endOfInput #-}

-- | Throw a 'ParseFailure' exception with the given 'String' error message
parseFail
    :: (Monad m, P.Proxy p)
    => String -> P.Pipe (E.EitherP SomeException p) (Maybe a) b (BufferT a m) r
parseFail str = E.throw (toException (ParseFailure str))
{-# INLINABLE parseFail #-}

-- | Override an existing parser's error message with a new one
(<??>)
    :: (Monad m, P.Proxy p)
    => P.Pipe (E.EitherP SomeException p) (Maybe a) b m r
       -- ^ Parser to modify
    -> String
       -- ^ New error message
    -> P.Pipe (E.EitherP SomeException p) (Maybe a) b m r
p <??> str= E.handle
    (\exc -> E.throw (case fromException exc of
        Just (ParseFailure _) -> toException (ParseFailure str)
        _                     -> exc ))
    p
{-# INLINABLE (<??>) #-}

infixl 0 <??>

-- | Parsing failed.  The 'String' describes the nature of the parse failure.
newtype ParseFailure = ParseFailure String deriving (Show, Typeable)

instance Exception ParseFailure

{- $reexport
    @Control.Proxy.Trans.Either@ exports run functions for the 'E.EitherP' proxy
    transformer.
-}
