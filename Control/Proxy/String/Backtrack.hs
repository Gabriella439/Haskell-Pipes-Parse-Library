module Control.Proxy.String.Backtrack (
    ) where

import Control.Monad
import Control.Proxy
import Control.Proxy.Parse.Backtrack
import Data.Char

drawChar :: (Monad m, ListT p) => ParseT p String m Char
drawChar = do
    skipWhile null
    c:str <- draw
    unDraw str
    return c

drawCharIf :: (Monad m, ListT p) => (Char -> Bool) -> ParseT p String m Char
drawCharIf pred = do
    c <- drawChar
    when (not (pred c)) $
        parseError ("drawCharIf " ++ show c ++ ": Character failed predicate")
    return c

drawCharN :: (Monad m, ListT p) => Int -> ParseT p String m String
drawCharN n0 = go id n0 where
    go diffStr n = do
        mstr <- silence (protect draw)
        case mstr of
            Nothing -> parseError $
                "drawCharN " ++ show n0 ++ ": Found " ++ show (n0 - n)
             ++ " characters"
            Just str -> do
                let len = length str
                if (len < n)
                    then go (diffStr . (str ++)) (n - len)
                    else do
                        let (prefix, suffix) = splitAt n str
                        unDraw suffix
                        return (diffStr prefix)

char :: (Monad m, ListT p) => Char -> ParseT p String m Char
char c = do
    c' <- drawChar
    when (c /= c') $ parseError ("char " ++ show c ++ ": Found " ++ show c')
    return c'

string :: (Monad m, ListT p) => String -> ParseT p String m String
string str = do
    str' <- drawCharN (length str)
    when (str /= str') $
        parseError ("string " ++ show str ++ ": Found " ++ show str')
    return str'

word :: (Monad m, ListT p) => ParseT p String m String
word = do
    strs <- drawWhile (all (not . isSpace))
    str  <- draw
    let (prefix, suffix) = break isSpace str
    unDraw suffix
    let w = concat $ strs ++ [prefix]
    when (null w) $ parseError ("word: Found " ++ show suffix)
    return w
