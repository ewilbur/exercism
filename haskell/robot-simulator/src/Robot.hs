module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

import Control.Arrow

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Bounded)

instance Enum Bearing where
  toEnum k = case k `mod` 4 of
               3 -> North
               2 -> East
               1 -> South
               0 -> West
  fromEnum b = case b of
                 North -> 3
                 East  -> 2
                 South -> 1
                 West  -> 0

data Robot = Robot
  { bearing :: Bearing
  , coordinates :: (Integer, Integer)
  }

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate = foldl execute
  where
    execute r c = case c of
                    'R' -> r { bearing = turnRight . bearing $ r }
                    'L' -> r { bearing = turnLeft . bearing $ r }
                    'A' -> r { coordinates = move (bearing r) . coordinates $ r}
    move North = second (+1)
    move South = second (subtract 1)
    move East  = first (+1)
    move West  = first (subtract 1)

turnLeft :: Bearing -> Bearing
turnLeft = succ

turnRight :: Bearing -> Bearing
turnRight = pred
