merge :: Ord a => [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = if x < y then
                          x : merge xs (y:ys)
                      else
                          y : merge (x:xs) ys

main :: IO ()
main = print $ merge [2,5,6] [1,3,4]
