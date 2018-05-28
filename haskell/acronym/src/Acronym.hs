module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate = (=<<) getAbbreviation . words

getAbbreviation :: String -> String
getAbbreviation xs'@(x:xs)
  | all isUpper xs' = [x]
  | otherwise  = toUpper <$> x : getAbbreviation' xs
    where
      getAbbreviation' (y:y':ys) = case generalCategory y of
                                     UppercaseLetter -> y : getAbbreviation' (y':ys)
                                     DashPunctuation -> y' : getAbbreviation' ys
                                     _ -> getAbbreviation' (y':ys)
      getAbbreviation' _ = []

getAbbreviation _ = []
