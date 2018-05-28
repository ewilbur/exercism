module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz k
  | k <= 0 = Nothing
  | even k = fmap (+1) (collatz (k `div` 2))
  | otherwise = fmap (+1) . collatz $ 3 * k + 1
