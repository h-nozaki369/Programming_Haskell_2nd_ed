isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = (x `elem` ys) && isChoice xs (rmItem x ys) 

rmItem :: Eq a => a -> [a] -> [a]
rmItem _ []     = []
rmItem x (y:ys)
    | x == y    = ys
    | otherwise = y : rmItem x ys
