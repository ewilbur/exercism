module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday deriving (Show, Eq, Read)

instance Enum Weekday where
    toEnum i =
        case mod i 7 of
            0 -> Sunday
            1 -> Monday
            2 -> Tuesday
            3 -> Wednesday
            4 -> Thursday
            5 -> Friday
            _ -> Saturday
    fromEnum Monday    = 1
    fromEnum Tuesday   = 2
    fromEnum Wednesday = 3
    fromEnum Thursday  = 4
    fromEnum Friday    = 5
    fromEnum Saturday  = 6
    fromEnum Sunday    = 7

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

type Year = Integer
type Month = Int

dayOfWeek :: Day -> Weekday
dayOfWeek (ModifiedJulianDay d) = toEnum $ fromInteger $ d + 3

meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay schedule weekday year month = (case schedule of
                                          First -> head
                                          Second -> head . tail
                                          Third -> head . tail . tail
                                          Fourth -> head . tail . tail . tail
                                          Last -> head . reverse
                                          Teenth -> head . filter isTeenth) $ filter p thisMonth'
  where
    thisMonth' = thisMonth year month
    p = (==weekday) . dayOfWeek
    isTeenth d = case toGregorian d of
                   (_,_,x) -> x > 12 && x < 20

thisMonth :: Year -> Month -> [Day]
thisMonth y m = fromGregorian y m <$> [1..gregorianMonthLength y m]
