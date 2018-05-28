module Beer (song) where

import Data.Char
import Control.Monad
import Data.List

song :: String
song = intercalate "\n\n" (fmap verse [99,98..0]) ++ "\n"

verse :: Int -> String
verse = liftM2 (++) firstLine (('\n' :) . secondLine)
  where
    firstLine k = (capitalize . bottles) k ++ wall ++ ", " ++ bottles k ++ "."
    capitalize (x:xs) = toUpper x : xs
    wall = " on the wall"
    bottles k
      | k == 0 = "no more bottles of beer"
      | k == 1 = "1 bottle of beer"
      | k < 0 = bottles 99
      | otherwise = show k ++ " bottles of beer"
    secondLine k = takeDown k ++ bottles (k-1) ++ wall ++ "."
    takeDown k
      | k == 0 = "Go to the store and buy some more, "
      | k == 1 = "Take it down and pass it around, "
      | otherwise = "Take one down and pass it around, "
