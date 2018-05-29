module Anagram (anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . isAnagram

isAnagram xs ys = xs' /= ys' && sort xs' == sort ys'
  where
    xs' = fmap toLower xs
    ys' = fmap toLower ys
