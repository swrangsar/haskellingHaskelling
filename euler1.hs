-- sum of multiples of 3 or 5 below 1000

answer = sum $ filter (\x -> mod x 3 == 0 || mod x 5 == 0) $ takeWhile (< 1000) [1..]

main = do
    putStrLn (show answer)
