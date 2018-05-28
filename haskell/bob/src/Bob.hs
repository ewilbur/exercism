module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isQuestion xs && isYelling xs = "Calm down, I know what I'm doing!"
  | isQuestion xs = "Sure."
  | isYelling xs = "Whoa, chill out!"
  | otherwise = "Whatever."


isQuestion :: String -> Bool
isQuestion = (== '?') . last . cleanUp
  where
    cleanUp :: String -> String
    cleanUp = unwords . words

isSilence :: String -> Bool
isSilence = all isSpace

isYelling :: String -> Bool
isYelling = both (all isUpper) (not . null) . filter isLetter
  where
    both f g = \ys -> (f ys) && (g ys)
