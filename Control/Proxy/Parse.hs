{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides utilities for handling end of input and push-back
module Control.Proxy.Parse (
    -- * Parsing proxy transformer
    ParseP,
    parseWith,

    -- * Primitive parsers
    drawMay,
    unDraw,

    -- * Parsers that cannot fail
    drawWhen,
    skipWhen,
    drawUpTo,
    skipUpTo,
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

    -- * Error messages
    ParseFailure(..),
    parseFail,
    (<??>),

    -- * Utilities
    justK,

    -- * Re-exports
    -- $reexport
    module Control.Proxy.Trans.Either
    ) where

import Control.Exception (SomeException, Exception, toException, fromException)
import qualified Control.Proxy as P
import Control.Proxy ((>>~), (/>/), (\>\))
import Control.Proxy.Parse.Internal (ParseP(ParseP, unParseP), get, put)
import qualified Control.Proxy.Trans.Either as E
import Control.Proxy.Trans.Either (runEitherP, runEitherK)
import qualified Control.Proxy.Trans.State as S
import Data.Typeable (Typeable)

-- | Unwrap 'ParseP' by providing a source of input
parseWith
    :: (Monad m, P.Proxy p)
    => (()  ->                  p a'      a  () b m r)
       -- ^ Source
    -> (() -> P.Pipe (ParseP i p) (Maybe b)    c m r)
       -- ^ Parser
    -> (() ->                  p a'      a  () c m r)
       -- ^ New Source
parseWith source parser
    = S.evalStateP [] . unParseP . (onlyK (P.mapP source) P.>-> parser)
  where
    onlyK k q = P.runIdentityP (do
        r <- P.IdentityP (k q) >>~ wrap
        P.respond Nothing
        return r )
    wrap a = do
        a' <- P.respond (Just a)
        a2 <- P.request a'
        wrap a2

-- | Request 'Just' one element or 'Nothing' if at end of input
drawMay :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) b m (Maybe a)
drawMay = do
    s <- get
    case s of
        []     -> do
            ma <- P.request ()
            case ma of
                Nothing -> put [ma]
                _       -> return ()
            return ma
        ma:mas -> do
            case ma of
                Nothing -> return ()
                Just a  -> put mas
            return ma
{-# INLINE drawMay #-}

-- | Push a single element into the leftover buffer
unDraw :: (Monad m, P.Proxy p) => a -> P.Pipe (ParseP a p) (Maybe a) b m ()
unDraw a = do
    mas <- get
    put (Just a:mas)
{-# INLINABLE unDraw #-}

-- | Draw an element only when it satisfies the predicate
drawWhen
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) b m (Maybe a)
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
skipWhen
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) b m ()
skipWhen pred = do
    ma <- drawMay
    case ma of
        Nothing -> return ()
        Just a  -> if (pred a)
            then return ()
            else unDraw a
{-# INLINABLE skipWhen #-}

-- | Draw up to the specified number of elements
drawUpTo :: (Monad m, P.Proxy p) => Int -> P.Pipe (ParseP a p) (Maybe a) b m [a]
drawUpTo = go id
  where
    go diffAs n = if (n > 0)
        then do
            ma <- drawMay
            case ma of
                Nothing -> return (diffAs [])
                Just a  -> go (diffAs . (a:)) $! n - 1
        else return (diffAs [])
{-# INLINABLE drawUpTo #-}

{-| Skip up to the specified number of elements

    Faster than 'drawUpTo' if you don't need the input
-}
skipUpTo :: (Monad m, P.Proxy p) => Int -> P.Pipe (ParseP a p) (Maybe a) b m ()
skipUpTo = go
  where
    go n = if (n > 0)
        then do
            ma <- drawMay
            case ma of
                Nothing -> return ()
                Just _  -> go $! n - 1
        else return ()
{-# INLINABLE skipUpTo #-}

-- | Request as many consecutive elements satisfying a predicate as possible
drawWhile
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) b m [a]
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
skipWhile
    :: (Monad m, P.Proxy p)
    => (a -> Bool) -> P.Pipe (ParseP a p) (Maybe a) b m ()
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
drawAll :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) b m [a]
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
skipAll :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) b m ()
skipAll = go
  where
    go = do
        ma <- drawMay
        case ma of
            Nothing -> return ()
            Just _  -> go
{-# INLINABLE skipAll #-}

-- | Return whether cursor is at end of input
isEndOfInput :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) b m Bool
isEndOfInput = do
    ma <- peek
    return (case ma of
        Nothing -> True
        _       -> False )
{-# INLINABLE isEndOfInput #-}

-- | Look ahead one element without consuming it
peek :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) b m (Maybe a)
peek = do
    ma <- drawMay
    case ma of
        Nothing -> return ()
        Just a  -> unDraw a
    return ma
{-# INLINABLE peek #-}

-- | Request a single element
draw
    :: (Monad m, P.Proxy p)
    => P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m a
draw = do
    ma <- drawMay
    case ma of
        Nothing -> parseFail "draw: End of input"
        Just a  -> return a
{-# INLINABLE draw #-}

-- | Skip a single element
skip
    :: (Monad m, P.Proxy p)
    => P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m ()
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
    -> P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m a
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
    -> P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m ()
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
    -> P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m [a]
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
    => Int -> P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m ()
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
endOfInput
    :: (Monad m, P.Proxy p)
    => P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m ()
endOfInput = do
    b <- isEndOfInput
    if b then return () else parseFail "endOfInput: Not end of input"
{-# INLINABLE endOfInput #-}

-- | Parsing failed.  The 'String' describes the nature of the parse failure.
newtype ParseFailure = ParseFailure String deriving (Show, Typeable)

instance Exception ParseFailure

-- | Throw a 'ParseFailure' exception with the given 'String' error message
parseFail
    :: (Monad m, P.Proxy p)
    => String -> P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m r
parseFail str = P.liftP (E.throw (toException (ParseFailure str)))
{-# INLINABLE parseFail #-}

-- | Override an existing parser's 'ParseFailure' error message with a new one
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

{-| Upgrade a proxy \'@K@\'leisli arrow to work with 'Maybe's

    The upgraded proxy handles 'Just's and auto-forwards 'Nothing's

> justK p1 >-> justK p2 = justK (p1 >-> p2)
>
> justK idT = idT
-}
justK
    :: (Monad m, P.ListT p)
    => (q -> p x a x b m r) -> (q -> p x (Maybe a) x (Maybe b) m r)
justK k = P.runIdentityK (up \>\ (P.identityK k />/ dn))
  where
    dn b = P.respond (Just b)
    up x = do
        ma <- P.request x
        case ma of
            Nothing -> do
                x2 <- P.respond Nothing
                up x2
            Just a  -> return a
{-# INLINABLE justK #-}

{- $reexport
    @Control.Proxy.Trans.Either@ exports run functions for the 'E.EitherP' proxy
    transformer.
-}
