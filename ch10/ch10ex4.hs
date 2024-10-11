adder :: IO ()
adder = do
    putStr "How many numbers? "
    s <- getLine
    total <- readAndSum (read s) 0
    putStr "The total is "
    putStrLn (show total)

readAndSum :: Int -> Int -> IO Int
readAndSum 0 total = return total
readAndSum n total = do
    m <- getLine
    readAndSum (n - 1) (total + read m)
