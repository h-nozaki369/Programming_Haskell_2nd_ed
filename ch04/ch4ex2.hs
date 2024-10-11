third1, third2, third3 :: [a] -> a
third1 = head . tail . tail
third2 xs = xs !! 2
third3 (x:y:z:xs) = z

main :: IO ()
main = do
    print $ third1 [1..5]
    print $ third2 [1..5]
    print $ third3 [1..5]
