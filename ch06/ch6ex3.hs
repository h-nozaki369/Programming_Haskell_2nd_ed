pow :: Int -> Int -> Int
pow x 0 = 1
pow x y = x * pow x (y-1)

main :: IO ()
main = print $ pow 2 3
