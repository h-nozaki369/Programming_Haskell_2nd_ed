and :: [Bool] -> [Boll]
and []     = True
and (x:xs) = x && and xs

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys)
    | x == y    = True
    | otherwise = elem x ys
