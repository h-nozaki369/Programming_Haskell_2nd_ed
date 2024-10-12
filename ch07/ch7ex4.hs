dec2int :: [Int] -> Int
dec2int xs = foldl (\y x -> y * 10 + x) 0 xs

main :: IO ()
main = print $ dec2int [2,3,4,5]
