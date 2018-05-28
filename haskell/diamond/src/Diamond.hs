module Diamond (diamond) where

import Data.Char


diamond :: Char -> Maybe [String]
diamond c
  | isUpper c = Just . symmetrify $ symmetrify <$> letterRows
  | otherwise = Nothing
    where
      letterNum = ord c - ord 'A'
      symmetrify xs = xs ++ tail (reverse xs)
      letterRows = take (letterNum + 1) <$> zipWith drop [1..] letterCycles
      letterCycles = cycle . (: replicate letterNum ' ') <$> ['A'..c]
