euclid :: Int -> Int -> Int
euclid x y
    | x == y    = x
    | x < y     = euclid x (y - x)
    | otherwise = euclid (x - y) x

main :: IO ()
main = print $ euclid 6 27
