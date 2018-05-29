module Roman (numerals) where

import Data.Char
import Debug.Trace

numerals :: Integer -> Maybe String
numerals = fmap concat . traverse toRoman . numeralForm

numeralForm :: (Integral a, Show a) => a -> [Int]
numeralForm = filter (/=0) . reverse . zipWith (*) [10^k | k <- [0..]] . reverse . fmap digitToInt . show

toRoman = sequence . toRoman'
  where
    toRoman' k
      | k > 3000 = [Nothing]
      | k >= 1000 = Just 'M' : toRoman' (k - 1000)
      | k == 900 = Just 'C' : Just 'M' : toRoman' (k - 900)
      | k >= 500 = Just 'D' : toRoman' (k - 500)
      | k == 400 = Just 'C' : Just 'D' : toRoman' (k - 400)
      | k >= 100 = Just 'C' : toRoman' (k - 100)
      | k == 90 = Just 'X' : Just 'C' : toRoman' (k - 90)
      | k >= 50 = Just 'L' : toRoman' (k - 50)
      | k == 40 = Just 'X' : Just 'L' : toRoman' (k - 40)
      | k >= 10 = Just 'X' : toRoman' (k - 10)
      | k == 9 = Just 'I' : Just 'X' : toRoman' (k - 9)
      | k >= 5 = Just 'V' : toRoman' (k - 5)
      | k == 4 = Just 'I' : Just 'V' : toRoman' (k - 4)
      | k > 0 = Just 'I' : toRoman' (k - 1)
      | k == 0 = []
