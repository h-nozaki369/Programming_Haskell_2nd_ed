qsort []      = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                   smaller = [a | a <- xs, a <= x]
                   larger  = [b | b <- xs, b > x]

main :: IO ()
main = print $ qsort [2, 2, 3, 1, 1]
