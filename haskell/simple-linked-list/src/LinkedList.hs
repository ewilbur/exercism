module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | Cons a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Cons x _) = x

fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: LinkedList a -> LinkedList a
next (Cons _ l) = l

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList l = rev l Nil
  where
    rev Nil x = x
    rev (Cons x l) a = rev l (Cons x a)

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons x l) = x : toList l
