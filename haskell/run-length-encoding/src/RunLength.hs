module RunLength (decode, encode) where

import Data.List (group)
import Data.Char
import Control.Monad

decode :: String -> String
decode = concatMap (uncurry replicate) . seperate
  where
    -- | Parses encoded string and returns list of tuples (numberOfTimesRepeated, character)
    seperate :: String -> [(Int,Char)]
    seperate [] = []
    seperate xs'@(x:xs) = (if isDigit x
                              then (read repetition, head rest)
                              else (1, head rest)) : seperate (tail rest)
      where
        repetition = takeWhile isDigit xs'
        rest = dropWhile isDigit xs'


encode :: String -> String
encode = concatMap f . group
  where
    f :: String -> String
    f [x] = [x]
    f xs'@(x:xs) = (show . length) xs' ++ [x]
