import System.IO

readLine :: IO String
readLine = do
    c <- getCh
    case c of
        '\n'   -> do
             putChar '\n'
             return ""
        '\DEL' -> do
             putChar '\b'
             cs <- readLine
             return cs
        _      -> do
             putChar c
             cs <- readLine
             return (c:cs)

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x
