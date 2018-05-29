module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import qualified Data.Vector as V
import Data.List (transpose)
import Control.Arrow
import Data.Maybe

type Matrix a = [[a]]

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing

rows :: Matrix a -> Int
rows = maybe 0 length . safeHead . transpose

column :: Int -> Matrix a -> V.Vector a
column x = V.fromList . flip (!!) x . transpose

flatten :: Matrix a -> V.Vector a
flatten = V.fromList . concat

fromList :: [[a]] -> Matrix a
fromList = id

fromString :: Read a => String -> Matrix a
fromString xs = fmap read . words <$> lines xs

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_,c) = taking c . concat
  where
    taking _ [] = []
    taking k m = take k m : taking k (drop k m)

row :: Int -> Matrix a -> V.Vector a
row = (V.fromList .) . flip (!!)

cols :: Matrix a -> Int
cols = maybe 0 length . safeHead

shape :: Matrix a -> (Int, Int)
shape = rows &&& cols
