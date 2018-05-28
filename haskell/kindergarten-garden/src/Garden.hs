module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List
import Control.Monad
import qualified Data.Map.Strict as M

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Student = String
type Garden = M.Map Student [Plant]

garden :: [String] -> Student -> Garden
garden = (M.fromList .) . (. groupPlants) . zip

lookupPlants :: Student -> Garden -> [Plant]
lookupPlants student garden = garden M.! student

groupPlants :: String -> [[Plant]]
groupPlants = concat . translateToPlants . groupStudentPlants . groupRows . lines
  where
    groupRows = traverse (groupsOf 2)
    groupStudentPlants = concatMap (fmap concat . transpose)
    translateToPlants = (traverse . traverse) toPlant
    toPlant c = case c of
                  'V' -> Just Violets
                  'R' -> Just Radishes
                  'C' -> Just Clover
                  'G' -> Just Grass
                  _   -> Nothing

    groupsOf k xs = traverse (isLength k) (groupsOf' k xs)
      where
        groupsOf' _ [] = []
        groupsOf' n ys = take n ys : groupsOf' n (drop n ys)
        isLength n ys  = if length ys == n then Just ys else Nothing
