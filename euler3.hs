isPrime :: Integer -> Bool
isPrime x = null $ filter (\y -> mod x y == 0) $ takeWhile (\y -> y*y <= x) [2..]

removeFactor :: Integer -> Integer -> Integer
removeFactor x n
    | (mod x n) == 0 = removeFactor (div x n) n
    | otherwise = x

primes :: [Integer]
primes = filter isPrime [2..]

primeFactors :: Integer -> Int -> [Integer]
primeFactors x n
     | x < p = []
     | mod x p == 0 = p:(primeFactors next (n+1))
     | otherwise = primeFactors x (n+1)
     where p = primes !! n
           next = removeFactor x p

bigNum = 600851475143
answer = maximum $ primeFactors bigNum 0

main = do
    putStrLn "The largest prime factor is: "
    putStrLn (show answer)