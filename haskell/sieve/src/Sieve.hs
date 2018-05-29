-- module Sieve (primesUpTo) where
module Main where

import Control.Arrow
import Debug.Trace
import System.Environment
import Control.DeepSeq

primesUpTo :: Integer -> [Integer]
primesUpTo n = fmap fst . filter snd . sieve (ceiling . sqrt . fromIntegral $ n) $ zip [2..n] (repeat True)

sieve _ [] = []
sieve 0 xs = xs
sieve n (x:xs) = x : sieve (n - 1) (gaps x xs)
  where
    gaps (y,True) = zipWith second (fmap (&&) . cycle . reverse $ False : replicate (fromIntegral y - 1) True)
    gaps _ = id

main = do
  args <- getArgs
  -- let bnd = head . read $ args
  return ()
