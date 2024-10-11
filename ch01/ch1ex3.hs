product' :: [Int] -> Int
product' []     = 1
product' (x:xs) = x * product' xs

main :: IO ()
main = print $ product' [2,3,4]
