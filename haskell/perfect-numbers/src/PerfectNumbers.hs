module PerfectNumbers (classify, Classification(..)) where

import Math.NumberTheory.ArithmeticFunctions

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify k
  | k <= 0 = Nothing
  | otherwise = case compare (2*k) (sum . divisors $ k) of
                  EQ -> Just Perfect
                  GT -> Just Deficient
                  LT -> Just Abundant
