module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Eq a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | (not . valid a b) c = Illegal
  | 0 `elem` [a,b,c] = Illegal
  | otherwise = triangleType' a b c
  where
    valid a b c = let [x,y,z] = sort [a,b,c] in
    triangleType' a b c
      | all (==a) [b,c] = Equilateral
      | a `elem` [b,c] || b == c = Isosceles
      | otherwise = Scalene
