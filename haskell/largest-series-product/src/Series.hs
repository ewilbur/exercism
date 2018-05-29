module Series (Error(..), largestProduct) where

import Data.Char
import Control.Monad

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)


largestProduct :: Int -> String -> Either Error Integer
largestProduct 0 _ = Right 1
largestProduct size digits = do
  validSpan size digits
  traverse validDigit digits
  Right $ if all (=='0') digits
             then 0
             else fromIntegral . maximum . getSequences size $ digits

ifError e b = if b then Right undefined else Left e

validSpan :: Int -> String -> Either Error b
validSpan k = ifError InvalidSpan . liftM2 (&&) (const (k > 0)) ((>= k) . length)

validDigit :: Char -> Either Error b
validDigit = liftM2 ifError InvalidDigit isDigit

getSequences k = concatMap lengthSeq . words . fmap f
  where
    f '0' = ' '
    f c = c
    lengthSeq xs
      | length xs < k = [0]
      | otherwise = product (fmap digitToInt (take k xs)) : lengthSeq (tail xs)
