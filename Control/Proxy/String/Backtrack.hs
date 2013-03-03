module Control.Proxy.String.Backtrack (
    -- * Single-character parsers
    drawChar,
    skipChar,
    drawCharIf,
    skipCharIf,

    -- * Efficient bulk parsers
    drawCharN,
    skipCharN,
    drawCharWhile,
    skipCharWhile,
    drawCharAll,

    -- * Exact matches
    char,
    string,

    -- * Groups
    word, 
    line
    ) where

import Control.Monad
import Control.Proxy
import Control.Proxy.Parse.Backtrack
import Data.Char

-- | Request a single character
drawChar :: (Monad m, ListT p) => ParseT p String m Char
drawChar = do
    skipWhile null
    c:str <- draw
    unDraw str
    return c

-- | Skip a single character
skipChar :: (Monad m, ListT p) => ParseT p String m ()
skipChar = void drawChar

-- | Request a single character that must satisfy the predicate
drawCharIf :: (Monad m, ListT p) => (Char -> Bool) -> ParseT p String m Char
drawCharIf pred = do
    c <- drawChar
    if (pred c) then return c else empty

-- | Skip a single character that must satisfy the predicate
skipCharIf :: (Monad m, ListT p) => (Char -> Bool) -> ParseT p String m ()
skipCharIf pred = void (drawCharIf pred)

-- | Request a fixed number of characters
drawCharN :: (Monad m, ListT p) => Int -> ParseT p String m String
drawCharN = go0
  where
    go0 n = if (n <= 0)
        then return ""
        else go1 id n
    go1 diffStr n = do
        mstr <- drawMay
        case mstr of
            Nothing  -> empty
            Just str -> do
                let len = length str
                if (len < n)
                    then go1 (diffStr . (str ++)) (n - len)
                    else do
                        let (prefix, suffix) = splitAt n str
                        unDraw suffix
                        return (diffStr prefix)

{-| Skip a fixed number of characters

    Faster than 'drawCharN' if you don't need the input
-}
skipCharN :: (Monad m, ListT p) => Int -> ParseT p String m ()
skipCharN = go0
  where
    go0 n = if (n <= 0)
        then return ()
        else go1 n
    go1 n = do
        mstr <- drawMay
        case mstr of
            Nothing  -> empty
            Just str -> do
                let len = length str
                if (len < n)
                    then go1 (n - len)
                    else do
                        let suffix = drop n str
                        unDraw suffix
                        return ()

-- | Request as many consecutive characters satisfying a predicate as possible
drawCharWhile
    :: (Monad m, ListT p) => (Char -> Bool) -> ParseT p String m String
drawCharWhile pred = go id
  where
    go diffStr = do
        mstr <- drawMay
        case mstr of
            Nothing  -> return (diffStr "")
            Just str -> do
                let (prefix, suffix) = span pred str
                if (null suffix)
                    then go (diffStr . (prefix ++))
                    else do
                        unDraw suffix
                        return (diffStr prefix)

{-| Skip as many consecutive characters satisfying a predicate as possible

    Faster than 'drawWhile' if you don't need the input
-}
skipCharWhile
    :: (Monad m, ListT p) => (Char -> Bool) -> ParseT p String m ()
skipCharWhile pred = go
  where
    go = do
        mstr <- drawMay
        case mstr of
            Nothing  -> return ()
            Just str -> do
                let suffix = dropWhile pred str
                if (null suffix)
                    then go
                    else unDraw suffix

drawCharAll :: (Monad m, ListT p) => ParseT p String m String
drawCharAll = go id
  where
    go diffStr = do
        mstr <- drawMay
        case mstr of
            Nothing  -> return (diffStr "")
            Just str -> go (diffStr . (str ++))

char :: (Monad m, ListT p) => Char -> ParseT p String m Char
char c = do
    c' <- drawChar
    if (c == c') then return c' else empty

string :: (Monad m, ListT p) => String -> ParseT p String m String
string str = do
    str' <- drawCharN (length str)
    if (str == str') then return str else empty

word :: (Monad m, ListT p) => ParseT p String m String
word = do
    skipCharWhile isSpace
    str <- drawCharWhile (not . isSpace)
    if (null str)
        then empty
        else return str

line :: (Monad m, ListT p) => ParseT p String m String
line = do
    str <- drawCharWhile (/= '\n')
    char '\n'
    return str
