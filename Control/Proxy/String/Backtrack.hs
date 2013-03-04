{-| 'String'-based parsers

    Note that 'String's are very inefficient, and I will release future separate
    packages with 'ByteString' and 'Text' operations.  I only include these to
    allow users to test simple parsing without requiring additional library
    dependencies.
-}
module Control.Proxy.String.Backtrack (
    -- * Single-character parsers
    draw,
    skip,
    drawIf,
    skipIf,

    -- * Efficient bulk parsers
    drawN,
    skipN,
    drawWhile,
    skipWhile,
    drawAll,
    skipAll,

    -- * Pushback
    unDraw,

    -- * Fail-safe parsers
    drawMay,
    peek,

    -- * Exact matches
    char,
    string,

    -- * Groups
    word, 
    line,

    -- * Numeric parsers
    decimal,
    signed,

    -- * End of input
    PB.endOfInput,
    PB.isEndOfInput,

    -- * Non-backtracking parsing
    PB.commit,

    -- * Run functions
    PB.evalParseT,

    -- * End-of-input utilities
    PB.only,
    PB.onlyK,
    PB.just,
    PB.justK,

    -- * Re-exports
    -- $reexport
    module Control.Applicative,
    module Control.Monad,
    module Data.Char,

    PB.few,
    PB.anything
    ) where

import Control.Applicative (
    Applicative(pure, (<*>), (<*), (*>)),
    Alternative(empty, (<|>), some, many),
    (<$>),
    (<$),
    (<**>),
    optional )
import Control.Monad (
    replicateM, replicateM_, MonadPlus(mzero, mplus), msum, mfilter, guard )
import qualified Control.Monad as M
import Control.Proxy
import qualified Control.Proxy.Parse.Backtrack as PB
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
draw :: (Monad m, ListT p) => PB.ParseT p String m Char
draw = do
    PB.skipWhile null
    c:str <- PB.draw
    PB.unDraw str
    return c

-- | Skip a single character
skip :: (Monad m, ListT p) => PB.ParseT p String m ()
skip = M.void draw

-- | Request a single character that must satisfy the predicate
drawIf :: (Monad m, ListT p) => (Char -> Bool) -> PB.ParseT p String m Char
drawIf pred = do
    c <- draw
    if (pred c) then return c else empty

-- | Skip a single character that must satisfy the predicate
skipIf :: (Monad m, ListT p) => (Char -> Bool) -> PB.ParseT p String m ()
skipIf pred = M.void (drawIf pred)

-- | Request a fixed number of characters
drawN :: (Monad m, ListT p) => Int -> PB.ParseT p String m String
drawN = go0
  where
    go0 n = if (n <= 0)
        then return ""
        else go1 id n
    go1 diffStr n = do
        mstr <- PB.drawMay
        case mstr of
            Nothing  -> empty
            Just str -> do
                let len = length str
                if (len < n)
                    then go1 (diffStr . (str ++)) (n - len)
                    else do
                        let (prefix, suffix) = splitAt n str
                        PB.unDraw suffix
                        return (diffStr prefix)

{-| Skip a fixed number of characters

    Faster than 'drawN' if you don't need the input
-}
skipN :: (Monad m, ListT p) => Int -> PB.ParseT p String m ()
skipN = go0
  where
    go0 n = if (n <= 0)
        then return ()
        else go1 n
    go1 n = do
        mstr <- PB.drawMay
        case mstr of
            Nothing  -> empty
            Just str -> do
                let len = length str
                if (len < n)
                    then go1 (n - len)
                    else do
                        let suffix = drop n str
                        PB.unDraw suffix
                        return ()

-- | Request as many consecutive characters satisfying a predicate as possible
drawWhile
    :: (Monad m, ListT p) => (Char -> Bool) -> PB.ParseT p String m String
drawWhile pred = go id
  where
    go diffStr = do
        mstr <- PB.drawMay
        case mstr of
            Nothing  -> return (diffStr "")
            Just str -> do
                let (prefix, suffix) = span pred str
                if (null suffix)
                    then go (diffStr . (prefix ++))
                    else do
                        PB.unDraw suffix
                        return (diffStr prefix)

{-| Skip as many consecutive characters satisfying a predicate as possible

    Faster than 'drawWhile' if you don't need the input
-}
skipWhile
    :: (Monad m, ListT p) => (Char -> Bool) -> PB.ParseT p String m ()
skipWhile pred = go
  where
    go = do
        mstr <- PB.drawMay
        case mstr of
            Nothing  -> return ()
            Just str -> do
                let suffix = dropWhile pred str
                if (null suffix) then go else PB.unDraw suffix

-- | Request the rest of the input
drawAll :: (Monad m, ListT p) => PB.ParseT p String m String
drawAll = concat <$> PB.drawAll

{-| Skip the rest of the input

    Faster than 'drawAll' if you don't need the input
-}
skipAll :: (Monad m, ListT p) => PB.ParseT p String m ()
skipAll = PB.skipAll

-- | Push back a single character into the leftover buffer
unDraw :: (Monad m, ListT p) => Char -> PB.ParseT p String m ()
unDraw c = PB.unDraw [c]

-- | Request 'Just' one element or 'Nothing' if at end of input
drawMay :: (Monad m, ListT p) => PB.ParseT p String m (Maybe Char)
drawMay = go
  where
    go = do
        mstr <- PB.drawMay
        case mstr of
            Nothing  -> return Nothing
            Just str -> case str of
                ""   -> go
                c:cs -> do
                    PB.unDraw cs
                    return (Just c)

-- | Look ahead one element without consuming it
peek :: (Monad m, ListT p) => PB.ParseT p String m (Maybe Char)
peek = do
    m <- drawMay
    case m of
        Nothing -> return m
        Just c  -> do
            unDraw c
            return m

-- | Match a specific 'Char'
char :: (Monad m, ListT p) => Char -> PB.ParseT p String m Char
char c = do
    c' <- draw
    if (c == c') then return c' else empty

-- | Match a specific 'String'
string :: (Monad m, ListT p) => String -> PB.ParseT p String m String
string str = do
    str' <- drawN (length str)
    if (str == str') then return str else empty

-- | Match a non-empty word, skipping any preceding whitespace
word :: (Monad m, ListT p) => PB.ParseT p String m String
word = do
    skipWhile isSpace
    str <- drawWhile (not . isSpace)
    if (null str) then empty else return str

-- | Match a line, consuming but not including the newline token
line :: (Monad m, ListT p) => PB.ParseT p String m String
line = do
    str <- drawWhile (/= '\n')
    char '\n'
    return str

-- | Parse an unsigned 'Integral' number
decimal :: (Monad m, ListT p, Integral r) => PB.ParseT p String m r
decimal = do
    digits <- drawWhile (\c -> c >= '0' && c <= '9')
    if (null digits)
        then empty
        else return $
                 foldl' (\n c -> n * 10 + fromIntegral (C.ord c - 48)) 0 digits

-- | Convert a an unsigned number parser to a signed number parser
signed
    :: (Monad m, ListT p, Num r)
    => PB.ParseT p String m r -> PB.ParseT p String m r
signed p = msum
    [ negate <$> (char '-' *> p)
    , char '+' *> p
    , p
    ]

{- $reexport
    @Control.Applicative@ exports useful combinators for 'Functor',
    'Applicative', and 'Alternative', like 'many', ('<|>'), and 'optional'.

    @Control.Monad@ exports useful combinators for 'Monad' and 'MonadPlus',
    like 'replicateM', 'msum', and 'mfilter'.

    @Data.Char@ exports all the 'isXXX' character classification functions.
-}
