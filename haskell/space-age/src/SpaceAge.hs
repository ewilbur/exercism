module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = toEarthYear seconds / planetScaleFactor planet
  where
    planetScaleFactor :: Planet -> Float
    planetScaleFactor Mercury = 0.2408467
    planetScaleFactor Venus   = 0.61519726
    planetScaleFactor Earth   = 1
    planetScaleFactor Mars    = 1.8808158
    planetScaleFactor Jupiter = 11.862615
    planetScaleFactor Saturn  = 29.447498
    planetScaleFactor Uranus  = 84.016846
    planetScaleFactor Neptune = 164.79132

    toEarthYear sec = sec / (365.25 * 24 * 60 * 60)
