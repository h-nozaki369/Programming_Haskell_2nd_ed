merge :: Ord a => [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = if x < y then
                          x : merge xs (y:ys)
                      else
                          y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs) where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
            where
                (ys,zs) = halve xs

main :: IO ()
main = do
    print $ msort [1]
    print $ msort [2,1]
    print $ msort [2,3,1]
    print $ msort [4,2,3,1]
