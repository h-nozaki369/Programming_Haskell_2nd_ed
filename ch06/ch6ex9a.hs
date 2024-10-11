-- 1st step
-- sum :: [Int] -> Int

-- 2nd step
-- sum [] =
-- sum (x:xs) =

-- 3rd step
-- sum [] = 0
-- sum (x:xs) =

-- 4th step
-- sum [] = 0
-- sum (x:xs) = x + sum xs

-- 5th step
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs
