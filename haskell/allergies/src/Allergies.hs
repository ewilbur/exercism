module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Bounded, Enum, Show)

allergies :: Int -> [Allergen]
allergies = flip filter [minBound..maxBound] . (. fromEnum) . testBit

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo = (. allergies) . elem
