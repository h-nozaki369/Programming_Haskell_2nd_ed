putStr' :: String -> IO ()
putStr' = sequence_ . (map putChar)
