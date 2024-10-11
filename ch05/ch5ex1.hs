main :: IO ()
main = print $ sum [x^2 | x <- [1..100]]
