module ETL (transform) where

import Data.Map (Map)
import Data.Map.Strict
import Data.Char
import Control.Arrow

transform :: Map a String -> Map Char a
transform = fromList . fmap (first toLower) . concatMap (uncurry (fmap . flip (,))) . assocs
