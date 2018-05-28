module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . fastNub $ do
  k <- factors
  [k, k*2 .. limit-1]

fastNub = fmap head . group . sort
