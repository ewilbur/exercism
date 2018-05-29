module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ b [] = b
foldl' f b (x:xs) = let z = f b x in seq z $ foldl' f z xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

length :: [a] -> Int
length = foldr (const (+1)) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x:acc else acc) []

infixr 5 ++
(++) :: [a] -> [a] -> [a]
(++) = flip (foldr (:))

concat :: [[a]] -> [a]
concat = foldr (++) []
