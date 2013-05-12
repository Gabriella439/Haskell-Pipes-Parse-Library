-- | This module provides utilities for handling end of input and push-back

{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}

module Control.Proxy.Parse (
    -- * Parsing proxy transformer
    ParseT,

    -- * Primitive parsers
    drawMay,
    unDraw,

    -- * Parsers that cannot fail
    drawWhen,
    skipWhen,
    drawUpToN,
    skipUpToN,
    drawWhile,
    skipWhile,
    drawAll,
    skipAll,
    isEndOfInput,
    peek,

    -- * Parsers that can fail
    draw,
    skip,
    drawIf,
    skipIf,
    drawN,
    skipN,
    endOfInput,

{-
    -- * Error messages
    parseFail,
    (<??>),

    -- * Parse exception
    ParseFailure(..),

    -- * Re-exports
    -- $reexport
    module Control.Proxy.Trans.Either,
-}
    ) where

import Control.Exception (SomeException, Exception, toException, fromException)
import Control.Monad (mzero)
import qualified Control.Proxy as P
import Control.Proxy ((>>~), (>->))
import Control.Proxy.Parse.Internal (ParseT(ParseT), runParseT)
import qualified Control.Proxy.Trans.Either as E
import Control.Proxy.Trans.Either (runEitherP, runEitherK)
import qualified Control.Proxy.Trans.State as S
import Data.Typeable (Typeable)

-- | Request 'Just' one element or 'Nothing' if at end of input
drawMay :: (Monad m) => ParseT [Maybe a] a m (Maybe a)
drawMay = ParseT $ P.liftP $ do
    s <- S.get
    case s of
        []     -> do
            ma <- P.request ()
            case ma of
                Nothing -> S.put [ma]
                _       -> return ()
            return ma
        ma:mas -> do
            case ma of
                Nothing -> return ()
                Just a  -> S.put mas
            return ma
{-# INLINE drawMay #-}

-- | Push a single element into the leftover buffer
unDraw :: (Monad m) => a -> ParseT [Maybe a] a m ()
unDraw a = ParseT $ P.liftP $ do
    mas <- S.get
    S.put (Just a:mas)
{-# INLINABLE unDraw #-}

-- | Draw an element only when it satisfies the predicate
drawWhen :: (Monad m) => (a -> Bool) -> ParseT [Maybe a] a m (Maybe a)
drawWhen pred = do
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
skipWhen :: (Monad m) => (a -> Bool) -> ParseT [Maybe a] a m ()
skipWhen pred = do
    ma <- drawMay
    case ma of
        Nothing -> return ()
        Just a  -> if (pred a)
            then return ()
            else unDraw a
{-# INLINABLE skipWhen #-}

-- | Draw up to the specified number of elements
drawUpToN :: (Monad m) => Int -> ParseT [Maybe a] a m [a]
drawUpToN = go id
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
skipUpToN :: (Monad m) => Int -> ParseT [Maybe a] a m ()
skipUpToN = go
  where
    go n = if (n > 0)
        then do
            ma <- drawMay
            case ma of
                Nothing -> return ()
                Just _  -> go $! n - 1
        else return ()
{-# INLINABLE skipUpToN #-}

-- | Request as many consecutive elements satisfying a predicate as possible
drawWhile :: (Monad m) => (a -> Bool) -> ParseT [Maybe a] a m [a]
drawWhile pred = go id
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
skipWhile :: (Monad m) => (a -> Bool) -> ParseT [Maybe a] a m ()
skipWhile pred = go
  where
    go = do
        ma <- drawMay
        case ma of
            Nothing -> return ()
            Just a  -> if (pred a)
                then go
                else unDraw a
{-# INLINABLE skipWhile #-}

-- | Request the rest of the input
drawAll :: (Monad m) => ParseT [Maybe a] a m [a]
drawAll = go id
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
skipAll :: (Monad m) => ParseT [Maybe a] a m ()
skipAll = go
  where
    go = do
        ma <- drawMay
        case ma of
            Nothing -> return ()
            Just _  -> go
{-# INLINABLE skipAll #-}

-- | Return whether cursor is at end of input
isEndOfInput :: (Monad m) => ParseT [Maybe a] a m Bool
isEndOfInput = do
    ma <- peek
    return (case ma of
        Nothing -> True
        _       -> False )
{-# INLINABLE isEndOfInput #-}

-- | Look ahead one element without consuming it
peek :: (Monad m) => ParseT [Maybe a] a m (Maybe a)
peek = do
    ma <- drawMay
    case ma of
        Nothing -> return ()
        Just a  -> unDraw a
    return ma
{-# INLINABLE peek #-}

-- | Request a single element
draw :: (Monad m) => ParseT [Maybe a] a m a
draw = do
    ma <- drawMay
    case ma of
        Nothing -> parseFail "draw: End of input"
        Just a  -> return a
{-# INLINABLE draw #-}

-- | Skip a single element
skip :: (Monad m) => ParseT [Maybe a] a m ()
skip = do
    ma <- drawMay
    case ma of
        Nothing -> parseFail "skip: End of input"
        Just _  -> return ()
{-# INLINABLE skip #-}

-- | Request a single element that must satisfy the predicate
drawIf :: (Monad m) => (a -> Bool) -> ParseT [Maybe a] a m a
drawIf pred = do
    ma <- drawMay
    case ma of
        Nothing -> parseFail "drawIf: End of input"
        Just a  -> if (pred a)
            then return a
            else parseFail "drawIf: Element failed predicate"
{-# INLINABLE drawIf #-}

-- | Skip a single element that must satisfy the predicate
skipIf :: (Monad m) => (a -> Bool) -> ParseT [Maybe a] a m ()
skipIf pred = do
    ma <- drawMay
    case ma of
        Nothing -> parseFail "skipIf: End of input"
        Just a  -> if (pred a)
            then return ()
            else parseFail "skipIf: Elemented failed predicate"
{-# INLINABLE skipIf #-}

-- | Request a fixed number of elements
drawN :: (Monad m) => Int -> ParseT [Maybe a] a m [a]
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
skipN :: (Monad m) => Int -> ParseT [Maybe a] a m ()
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

-- | Match end of input without consuming it
endOfInput :: (Monad m) => ParseT [Maybe a] a m ()
endOfInput = do
    b <- isEndOfInput
    if b then return () else parseFail "endOfInput: Not end of input"
{-# INLINABLE endOfInput #-}

-- | Fail parsing with a 'String' error message
parseFail :: (Monad m) => String -> ParseT s a m b
parseFail _ = mzero
{-# INLINABLE parseFail #-}

{-
-- | Override an existing parser's error message with a new one
(<??>)
    :: (Monad m, P.Proxy p)
    => P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m r
       -- ^ Parser to modify
    -> String
       -- ^ New error message
    -> P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m r
p <??> str= P.hoistP
    (E.handle (\exc -> E.throw (case fromException exc of
        Just (ParseFailure _) -> toException (ParseFailure str)
        _                     -> exc )))
    p
{-# INLINABLE (<??>) #-}

infixl 0 <??>

-- | Parsing failed.  The 'String' describes the nature of the parse failure.
newtype ParseFailure = ParseFailure String deriving (Show, Typeable)

instance Exception ParseFailure
-}

{- $reexport
    The @Control.Proxy.Trans.Either@ re-export provides 'E.runEitherP',
    'E.runEitherK', and 'E.EitherP'.
-}
