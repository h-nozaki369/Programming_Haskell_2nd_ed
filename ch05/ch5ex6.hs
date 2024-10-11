factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [m | m <- [1..n], sum (init (factors m)) == m]

main :: IO ()
main = print $ perfects 500
