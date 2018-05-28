module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA = traverse transcribeAmino
  where
    transcribeAmino c = case c of
                          'G' -> Just 'C'
                          'C' -> Just 'G'
                          'T' -> Just 'A'
                          'A' -> Just 'U'
                          _ -> Nothing
