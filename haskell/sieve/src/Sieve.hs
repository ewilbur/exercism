-- module Sieve (primesUpTo) where
module Sieve where

import Control.Arrow

primesUpTo :: Integer -> [Integer]
primesUpTo n = fmap fst . filter snd . sieve (ceiling . sqrt . fromIntegral $ n) $ zip [2..n] (repeat True)

sieve _ [] = []
sieve 0 xs = xs
sieve n (x:xs) = x : sieve (n - 1) (gaps x xs)
  where
    gaps (y,True) = zipWith second (fmap (&&) . cycle . reverse $ False : replicate (fromIntegral y - 1) True)
    gaps _ = id
