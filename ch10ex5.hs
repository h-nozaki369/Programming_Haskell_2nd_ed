adder :: IO ()
adder = do
    putStr "How many numbers? "
    s <- getLine
    strNums :: [String] <- sequence $ replicate (read s) getLine
    let total :: Int = sum (map read strNums)
    putStr "The total is "
    putStrLn (show total)
