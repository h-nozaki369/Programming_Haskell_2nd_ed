import Data.Char

let2int :: Char -> Int -> Int
let2int c base = ord c - base

int2let :: Int -> Int -> Char
int2let n base = chr (base + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c (ord 'a') + n) `mod` 26) (ord 'a')
          | isUpper c = int2let ((let2int c (ord 'A') + n) `mod` 26) (ord 'A')
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

main :: IO ()
main = do
    print $ encode 3 "haskell is fun"
    print $ encode (-3) "kdvnhoo lv ixq"
    print $ encode 3 "HASKELL is fun"
    print $ encode (-3) "KDVNHOO lv ixq"
