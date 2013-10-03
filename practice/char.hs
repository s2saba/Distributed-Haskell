import Data.Char

--(*) :: Char -> Char -> Char
--a * b = chr $ (ord a) Prelude.* (ord b)

instance Num Char where
x + y = chr $ (ord x) + (ord y)
x * y = chr $ (ord x) * (ord y)
x - y = chr $ (ord x) - (ord y)
negate x = chr $ (- (ord x))
abs x = x
signum x = x
fromInteger = chr
