{-| 'String'-based parsers

    Note that 'String's are very inefficient, and I will release future separate
    packages with 'ByteString' and 'Text' operations.  I only include these to
    allow users to test simple parsing without requiring additional library
    dependencies.
-}
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
    skipCharAll,

    -- * Pushback
    unDrawChar,

    -- * Fail-safe parsers
    drawCharMay,
    peekChar,

    -- * Exact matches
    char,
    string,

    -- * Groups
    word, 
    line,

    -- * Re-exports
    -- $reexport
    module Data.Char
    ) where

import Control.Monad
import Control.Proxy
import Control.Proxy.Parse.Backtrack
import Data.Char (
    isControl,
    isSpace,
    isLower,
    isUpper,
    isAlpha,
    isAlphaNum,
    isPrint,
    isDigit,
    isOctDigit,
    isHexDigit,
    isLetter,
    isMark,
    isNumber,
    isPunctuation,
    isSymbol,
    isSeparator,
    isAscii,
    isLatin1,
    isAsciiUpper,
    isAsciiLower )
import qualified Data.Char as C
import Data.List (foldl')

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

    Faster than 'drawCharWhile' if you don't need the input
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
                if (null suffix) then go else unDraw suffix

-- | Request the rest of the input
drawCharAll :: (Monad m, ListT p) => ParseT p String m String
drawCharAll = concat <$> drawAll

{-| Skip the rest of the input

    Faster than 'drawCharAll' if you don't need the input
-}
skipCharAll :: (Monad m, ListT p) => ParseT p String m ()
skipCharAll = skipAll

-- | Push back a single character into the leftover buffer
unDrawChar :: (Monad m, ListT p) => Char -> ParseT p String m ()
unDrawChar c = unDraw [c]

-- | Request 'Just' one element or 'Nothing' if at end of input
drawCharMay :: (Monad m, ListT p) => ParseT p String m (Maybe Char)
drawCharMay = go
  where
    go = do
        mstr <- drawMay
        case mstr of
            Nothing  -> return Nothing
            Just str -> case str of
                ""   -> go
                c:cs -> do
                    unDraw cs
                    return (Just c)

-- | Look ahead one element without consuming it
peekChar :: (Monad m, ListT p) => ParseT p String m (Maybe Char)
peekChar = do
    m <- drawCharMay
    case m of
        Nothing -> return m
        Just c  -> do
            unDrawChar c
            return m

-- | Match a specific 'Char'
char :: (Monad m, ListT p) => Char -> ParseT p String m Char
char c = do
    c' <- drawChar
    if (c == c') then return c' else empty

-- | Match a specific 'String'
string :: (Monad m, ListT p) => String -> ParseT p String m String
string str = do
    str' <- drawCharN (length str)
    if (str == str') then return str else empty

-- | Match a non-empty word, skipping any preceding whitespace
word :: (Monad m, ListT p) => ParseT p String m String
word = do
    skipCharWhile isSpace
    str <- drawCharWhile (not . isSpace)
    if (null str) then empty else return str

-- | Match a line, consuming but not including the newline token
line :: (Monad m, ListT p) => ParseT p String m String
line = do
    str <- drawCharWhile (/= '\n')
    char '\n'
    return str

decimal :: (Monad m, ListT p, Integral r) => ParseT p String m r
decimal = do
    digits <- drawCharWhile (\c -> c >= '0' && c <= '9')
    if (null digits)
        then empty
        else return $
                 foldl' (\n c -> n * 10 + fromIntegral (C.ord c - 48)) 0 digits

signed
    :: (Monad m, ListT p, Num r) => ParseT p String m r -> ParseT p String m r
signed p = msum
    [ negate <$> (char '-' *> p)
    , char '+' *> p
    , p
    ]

{- $reexport
    The @Data.Char@ re-export includes all the 'isXXX' classification functions.
-}
