module Scrabble (scoreLetter, scoreWord) where

import Control.Arrow
import Data.Char

scoreLetter :: Char -> Integer
scoreLetter = snd . head . flip filter letterScores . (fst .) . first . elem . toUpper
  where
    letterScores = zip ["?","AEIOULNRST","DG","BCMP","FHVWY","K","JX","QZ"] [0,1,2,3,4,5,8,10]

scoreWord :: String -> Integer
scoreWord = sum . fmap scoreLetter
