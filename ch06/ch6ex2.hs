sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

main :: IO ()
main = print $ sumdown 3
