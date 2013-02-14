-- calculate Pi using Chudnovsky algorithm

type Fraction = (Integer, Integer)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)

numerator :: Integer -> Integer
numerator k
    | k >= 0 = ((-1)^k) * factorial (6 * k) * (13591409 + (545140134 * k))

denominator :: Integer -> Integer
denominator k
    | k >= 0 = a * b * c
    where a = factorial (3 * k)
          b = ((factorial k)^3)
          c = truncate (640320 ** ((3 * (fromIntegral k)) + (3/2)))


term :: Integer -> Fraction
term k
    | k >= 0 = (a, b)
    where a = numerator k
          b = denominator k

addFraction :: Fraction -> Fraction -> Fraction
addFraction (0, _) (c, d) = (c, d)
addFraction (a, b) (0, _) = (a, b)
addFraction (a, b) (c, d) = (num, den)
    where num = (a * d) + (b * c)
          den = (b * d)

sumFraction :: [Fraction] -> Fraction
sumFraction [] = (0, 1)
sumFraction (x:xs) = addFraction x (sumFraction xs)

scaleFraction :: Integer -> Fraction -> Fraction
scaleFraction n (a, b) = (n * a, b)

          

inversePi :: Int -> Fraction
inversePi 0 = scaleFraction 12 (term 0)
inversePi n
    | n > 0 =  scaleFraction 12 a
    where a = addFraction (term 0) (sumFraction $ map term $ take n [1..])


getPi :: Int -> Double
getPi n
    | n >= 0 = estimatedPi
    where (a, b) = inversePi n
          estimatedPi = (fromIntegral b) / (fromIntegral a)