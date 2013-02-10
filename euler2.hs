-- sum of even fibonacci numbers not greater than 4 million
fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

result :: Integer
result = sum $ filter even $ takeWhile (<= 4000000) fibs

main = do
    putStrLn "The sum of even fibonacci numbers is: "
    putStrLn (show result)
