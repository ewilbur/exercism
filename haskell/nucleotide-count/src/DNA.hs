module DNA (nucleotideCounts) where

import Data.Map (Map)
import Data.Map.Strict

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts [] = Right . fromList $ [('A',0), ('T', 0), ('C', 0), ('G', 0)]
nucleotideCounts (x:xs)
  | x `elem` "ATCG" = insertWith (+) x 1 <$> nucleotideCounts xs
  | otherwise  = Left "Unrecognized amino acid!"
