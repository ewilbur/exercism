module Base (Error(..), rebase) where

import Data.List

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

data BaseType = Output | Input deriving Eq

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inB outB digits = do
  validBase inB Input
  validBase outB Output
  traverse (validBaseDigit inB) digits
  Right . reverse . evalToBase outB . evalFromBase inB $ digits

evalFromBase :: Integral a => a -> [a] -> a
evalFromBase base = sum . zipWith (*) [base ^ k | k <- [0..]] . reverse

evalToBase :: Integral a => a -> a -> [a]
evalToBase _ 0 = []
evalToBase base n = digit : evalToBase base (div (n - digit) base)
  where
    digit = rem n base

validBase :: Integral a => a -> BaseType -> Either (Error a) a
validBase a b
  | a > 1 = Right a
  | b == Output = Left InvalidOutputBase
  | otherwise = Left InvalidInputBase

validBaseDigit :: Integral a => a -> a -> Either (Error a) a
validBaseDigit base inputDigit
  | inputDigit < 0 || inputDigit >= base = Left (InvalidDigit inputDigit)
  | otherwise = Right inputDigit
