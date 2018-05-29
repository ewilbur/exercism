module ProteinTranslation(proteins) where

data Protein = Methionine
             | Phenylalanine
             | Leucine
             | Serine
             | Tyrosine
             | Cysteine
             | Tryptophan
             | Stop deriving (Show, Eq)

readProtein :: String -> Maybe Protein
readProtein xs@[x,y,z]
  | xs `elem` ["AUG"]                   = Just Methionine
  | xs `elem` ["UUU","UUC"]             = Just Phenylalanine
  | xs `elem` ["UUA","UUG"]             = Just Leucine
  | xs `elem` ["UCU","UCC","UCA","UCG"] = Just Serine
  | xs `elem` ["UAU", "UAC"]            = Just Tyrosine
  | xs `elem` ["UGU", "UGC"]            = Just Cysteine
  | xs `elem` ["UGG"]                   = Just Tryptophan
  | xs `elem` ["UAA", "UAG", "UGA"]     = Just Stop
  | otherwise                           = Nothing

readProtein xs = Nothing

groupsOf _ [] = []
groupsOf k xs = take k xs : groupsOf k (drop k xs)

proteins :: String -> Maybe [String]
proteins = fmap (fmap show . takeWhile (/= Stop)) . traverse readProtein . groupsOf 3
