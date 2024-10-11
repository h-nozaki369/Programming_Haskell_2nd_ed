last1 xs = xs !! (length xs - 1)
last2 xs = head $ reverse xs
last3 xs = head $ drop (length xs - 1) xs

main :: IO ()
main = do
    print $ last1 [1..5]
    print $ last2 [1..5]
    print $ last3 [1..5]
