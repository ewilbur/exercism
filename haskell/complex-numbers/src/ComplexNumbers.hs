module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs)
import Control.Arrow

-- Data definition -------------------------------------------------------------
-- data Complex a = Dummy deriving(Eq, Show)
type Complex a = (a,a)

complex :: (a, a) -> Complex a
complex = id

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate = second negate

abs :: Floating a => Complex a -> a
abs = sqrt . tupSum . (***) (^2) (^2)
  where
    tupSum (a,b) = a + b

real :: Num a => Complex a -> a
real = fst

imaginary :: Num a => Complex a -> a
imaginary = snd

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (a,b) (x,y) = (a*x - b*y, a*y + b*x)

add :: Num a => Complex a -> Complex a -> Complex a
add (a,b) (x,y) = (a + x, b + y)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (a,b) (x,y) = (a - x, b - y)

-- div :: Fractional a => Complex a -> Complex a -> Complex a
div c1 c2 = (c1 `mul` conjugate c2) `div'` real (c2 `mul` conjugate c2)
  where
    div' (a,b) x = (a/x, b/x)
