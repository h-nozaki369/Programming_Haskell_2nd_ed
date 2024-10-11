-- 1st step
-- last :: [a] -> a

-- 2nd step
-- last [x] =
-- last (x:xs) =

-- 3rd step
-- last [x] = x
-- last (x:xs) =

-- 4th step
-- last [x] = x
-- last (x:xs) = last xs

-- 5th step
last :: [a] -> a
last [x] = x
last (x:xs) = last xs
