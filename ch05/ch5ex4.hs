replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

main :: IO ()
main = print $ replicate' 3 True
