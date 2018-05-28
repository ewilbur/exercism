module Pangram (isPangram) where

import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram = (==) ['a'..'z'] . fmap head . group . sort . fmap toLower . filter isAlpha
