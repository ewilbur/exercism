module Phone (number) where

import Data.Char
import Control.Monad

number :: String -> Maybe String
number = mbSplitNumber . filter isDigit >=> mbRemoveCountryCode >=> mbValidNumber

mbSplitNumber :: [a] -> Maybe [[a]]
mbSplitNumber = return . fmap reverse . ([drop 7, take 3 . drop 4, take 4] <*>) . return . reverse

mbRemoveCountryCode (xs:xss)
  | length xs == 4 && head xs == '1' = Just $ tail xs : xss
  | length xs == 3 = Just (xs : xss)
  | otherwise = Nothing

mbRemoveCountryCode _ = Nothing

mbValidNumber num@[xs,ys,_]
  | length xs == length ys && head xs `elem` ['2'..'9'] && head ys `elem` ['2'..'9'] = Just . concat $ num
  | otherwise = Nothing

mbValidNumber _ = Nothing
