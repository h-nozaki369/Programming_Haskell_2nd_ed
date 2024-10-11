halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where n = length xs `div` 2 

main :: IO ()
main = print $ halve [1..6]
