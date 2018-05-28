module School (School, add, empty, grade, sorted) where

import qualified Data.IntMap.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Control.Arrow (second)

type Names = [String]
type Student = String
type Grade = Int
type School = M.IntMap Names

add :: Grade -> Student -> School -> School
add gradeNum  = M.insertWith (++) gradeNum . return

empty :: School
empty = M.empty

grade :: Grade -> School -> Names
grade gradeNum = sort . fromMaybe [] . flip (M.!?) gradeNum

sorted :: School -> [(Grade, Names)]
sorted = fmap (second sort) . M.toList
