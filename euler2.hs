-- sum of even fibonacci numbers not greater than 4 million
fibs :: [Integer]
fibs = 0:1:[a+b | (a,b) <- zip fibs (tail fibs)]

result :: Integer
result = sum $ takeWhile (<= 4000000) $ filter even fibs

main = do
    putStrLn "The sum of even fibonacci numbers is: "
    putStrLn (show result)
