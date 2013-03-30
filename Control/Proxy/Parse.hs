{-# LANGUAGE DeriveDataTypeable #-}

-- | This module defines the core machinery handling push-back and end of input.
module Control.Proxy.Parse (
    -- * Non-backtracking parsing proxy transformer
    ParseP,

    -- * Parse exception
    ParseFailure(..),

    -- * Primitive parsers
    drawMay,
    unDraw,

    -- * Single-element parsers
    draw,
    skip,
    drawIf,
    skipIf,
    peek,

    -- * Efficient bulk parsers
    drawN,
    skipN,
    drawWhile,
    skipWhile,
    drawAll,
    skipAll,

    -- * End of input
    endOfInput,
    isEndOfInput,

    -- * Error messages
    (<??>),

    -- * Run functions
    runParseP,
    runParseK,

    -- * End of input utilities
    onlyP,
    onlyK,
    justP,
    justK,

    -- * Re-exports
    -- $reexport
    module Control.Proxy.Trans.Either
    ) where

import Control.Exception (SomeException, Exception, toException, fromException)
import qualified Control.Proxy as P
import Control.Proxy ((>>~), (//>), (>\\))
import Control.Proxy.Parse.Internal (ParseP(ParseP, unParseP), get, put)
import qualified Control.Proxy.Trans.Either as E
import Control.Proxy.Trans.Either (runEitherP, runEitherK)
import qualified Control.Proxy.Trans.State as S
import Data.Typeable (Typeable)

-- | Parsing failed.  The 'String' describes the nature of the parse failure.
newtype ParseFailure = ParseFailure String deriving (Show, Typeable)

instance Exception ParseFailure

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

-- | Request a single element
draw
    :: (Monad m, P.Proxy p)
    => P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m a
draw = do
    ma <- drawMay
    case ma of
        Nothing -> throwStr "draw: End of input"
        Just a  -> return a
{-# INLINABLE draw #-}

-- | Skip a single element
skip
    :: (Monad m, P.Proxy p)
    => P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m ()
skip = do
    ma <- drawMay
    case ma of
        Nothing -> throwStr "skip: End of input"
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
        Nothing -> throwStr "drawIf: End of input"
        Just a  -> if (pred a)
            then return a
            else throwStr "drawIf: Element failed predicate"
{-# INLINABLE drawIf #-}

-- | Skip a single element that must satisfy the predicate
skipIf
    :: (Monad m, P.Proxy p)
    => (a -> Bool)
    -> P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m ()
skipIf pred = do
    ma <- drawMay
    case ma of
        Nothing -> throwStr "skipIf: End of input"
        Just a  -> if (pred a)
            then return ()
            else throwStr "skipIf: Elemented failed predicate"
{-# INLINABLE skipIf #-}

-- | Look ahead one element without consuming it
peek :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) b m (Maybe a)
peek = do
    ma <- drawMay
    case ma of
        Nothing -> return ()
        Just a  -> unDraw a
    return ma
{-# INLINABLE peek #-}

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
                Nothing -> throwStr (
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
                Nothing -> throwStr (
                    "skipN " ++ show n0 ++ ": Found only " ++ show (n0 - n)
                 ++ " elements" )
                Just _  -> go $! n - 1
        else return ()
{-# INLINABLE skipN #-}

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

-- | Match end of input without consuming it
endOfInput
    :: (Monad m, P.Proxy p)
    => P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m ()
endOfInput = do
    b <- isEndOfInput
    if b then return () else throwStr "endOfInput: Not end of input"
{-# INLINABLE endOfInput #-}

-- | Return whether cursor is at end of input
isEndOfInput :: (Monad m, P.Proxy p) => P.Pipe (ParseP a p) (Maybe a) b m Bool
isEndOfInput = do
    ma <- peek
    return (case ma of
        Nothing -> True
        _       -> False )
{-# INLINABLE isEndOfInput #-}

-- | Emit a diagnostic message and abort parsing
throwStr
    :: (Monad m, P.Proxy p)
    => String -> P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m r
throwStr str = P.liftP (E.throw (toException (ParseFailure str)))
{-# INLINABLE throwStr #-}

-- | Override error message
(<??>)
    :: (Monad m, P.Proxy p)
    => P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m r
       -- ^ Parser to modify
    -> String
       -- ^ New default error message
    -> P.Pipe (ParseP a (E.EitherP SomeException p)) (Maybe a) b m r
p <??> str= P.hoistP
    (E.handle (\exc -> E.throw (case fromException exc of
        Just (ParseFailure _) -> toException (ParseFailure str)
        _                     -> exc )))
    p
{-# INLINABLE (<??>) #-}

infixl 0 <??>

{-| Evaluate a non-backtracking parser, returning the result or failing with a
    'ParseFailure' exception.
-}
runParseP :: (Monad m, P.Proxy p) => ParseP i p a' a b' b m r -> p a' a b' b m r
runParseP p = S.evalStateP [] (unParseP p)
{-# INLINABLE runParseP #-}

{-| Evaluate a non-backtracking parser \'@K@\'leisli arrow, returning the result
    or failing with a 'ParseFailure' exception.
-}
runParseK
    :: (Monad m, P.Proxy p)
    => (q -> ParseP i p a' a b' b m r) -> (q -> p a' a b' b m r)
runParseK k q = runParseP (k q)
{-# INLINABLE runParseK #-}

{- $reexport
    @Control.Monad.Trans.Either@ exports run functions for the 'E.EitherP' proxy
    transformer.
-}

-- | Wrap a proxy's output in 'Just' and finish with a 'Nothing'
onlyP :: (Monad m, P.Proxy p) => p a' a b' b m r -> p a' a b' (Maybe b) m r
onlyP p = P.runIdentityP (do
    r <- P.IdentityP p >>~ wrap
    P.respond Nothing
    return r )
  where
    wrap a = do
        a' <- P.respond (Just a)
        a2 <- P.request a'
        wrap a2
{-# INLINABLE onlyP #-}

{-| Wrap a proxy \'@K@\'leisli arrow's output in 'Just' and finish with a
    'Nothing'
-}
onlyK
    :: (Monad m, P.Proxy p)
    => (q -> p a' a b' b m r) -> (q -> p a' a b' (Maybe b) m r)
onlyK k q = onlyP (k q)
{-# INLINABLE onlyK #-}

{-| Upgrade a proxy to work with 'Maybe's

    The upgraded proxy handles 'Just's and auto-forwards 'Nothing's
-}
justP :: (Monad m, P.ListT p) => p x a x b m r -> p x (Maybe a) x (Maybe b) m r
justP p = P.runIdentityP (up >\\ (P.IdentityP p //> dn))
  where
    dn b = P.respond (Just b)
    up x = do
        ma <- P.request x
        case ma of
            Nothing -> do
                x2 <- P.respond Nothing
                up x2
            Just a  -> return a
{-# INLINABLE justP #-}

{-| Upgrade a proxy \'@K@\'leisli arrow to work with 'Maybe's

    The upgraded proxy handles 'Just's and auto-forwards 'Nothing's
-}
justK
    :: (Monad m, P.ListT p)
    => (q -> p x a x b m r) -> (q -> p x (Maybe a) x (Maybe b) m r)
justK k q = justP (k q)
{-# INLINABLE justK #-}
