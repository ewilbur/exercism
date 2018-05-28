module IsbnVerifier (isbn) where

import Data.Char
import Control.Monad
import Data.Maybe
import Data.Bool.HT

type ISBN = String

isbn = fromMaybe False . (convertISBN10 >=> checkConvertedISBN)
  where
    convertISBN10 xs = if length (cleaned xs) == 10 && all isDigit (init . cleaned $ xs)
                          then Just . fmap isbnDigitToInt . cleaned $ xs
                          else Nothing
    cleaned = filter isISBNDigit
    checkConvertedISBN = return . (== 0) . flip mod 11 . sum . zipWith (*) [1..] . reverse
    isISBNDigit = liftM2 (||) isDigit (('X' ==) . toUpper)
    isbnDigitToInt = flip (liftM2 if' isDigit digitToInt) 10
