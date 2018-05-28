module Raindrops (convert) where

convert :: Int -> String
convert n
  | elem 0 divList = concat $ zipWith (\x y -> if y == 0 then x else "") ["Pling", "Plang", "Plong"] divList
  | otherwise = show n
    where
      divList = mod <$> [n] <*> [3,5,7]
