module Clock (clockHour, clockMin, fromHourMin, toString) where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)

data Clock = Clock
  { clockHour :: Int
  , clockMin :: Int
  } deriving (Show, Eq)

instance Enum Clock where
  toEnum = fromInteger . fromIntegral
  fromEnum (Clock a b) = (a * 60) + b

instance Num Clock where
  (+) = clockArith (+)
  (-) = clockArith (-)
  (*) = clockArith (*)
  fromInteger = fromHourMin 0 . fromIntegral
  abs = id
  signum (Clock 0 0) = 0
  signum _ = 1

clockArith f c1 c2 = toEnum (fromEnum c1 `f` fromEnum c2)

fromHourMin :: Int -> Int -> Clock
fromHourMin h m
  | m >= 60 = fromHourMin (h + 1) (m - 60)
  | m < 0 = fromHourMin (h - 1) (m + 60)
  | otherwise = Clock (h `mod` 24) m

toString :: Clock -> String
toString = formatTime . bimap' show . tupleTime
  where
    formatTime (h,m) = f h ++ ":" ++ f m
    f x = if length x == 1 then '0':x else x
    tupleTime = clockHour &&& clockMin
    bimap' f = bimap f f
