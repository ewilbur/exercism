module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar

type Weekday = DayOfWeek
type Year = Integer
type Month = Int

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

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
