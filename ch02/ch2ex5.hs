init1 xs = take (length xs - 1) xs
init2 xs = reverse $ tail $ reverse xs
init3 [x] = []
init3 (x:xs) = x : init3 xs

main :: IO ()
main = do
    print $ init1 [1..5]
    print $ init2 [1..5]
    print $ init3 [1..5]
