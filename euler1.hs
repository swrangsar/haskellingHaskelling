-- multiples of 3 or 5 below 1000
isMultipleOf3or5 :: Integer -> Bool
isMultipleOf3or5 n = if (mod n 3 == 0 || mod n 5 == 0)
                        then True
                        else False

answer = sum $ filter isMultipleOf3or5 $ takeWhile (< 1000) [1..]

main = do
    putStrLn (show answer)
