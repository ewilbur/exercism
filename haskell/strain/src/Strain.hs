module Strain (keep, discard) where

if' p x y = if p then x else y

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard p (x:xs) = if' (p x) (discard p xs) (x : discard p xs)

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs) = if' (p x) (x : keep p xs) (keep p xs)
