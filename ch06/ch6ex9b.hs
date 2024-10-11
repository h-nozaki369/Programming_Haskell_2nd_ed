-- 1st step
-- take :: Int -> [a] -> [a]

-- 2nd step
-- take 0 xs =
-- take n (x:xs) =

-- 3rd step
-- take 0 xs = []
-- take n (x:xs) =

-- 4th step
-- take 0 xs = []
-- take n (x:xs) = x : take (n-1) xs

-- 5th step
take :: Int -> [a] -> [a]
take 0 xs = []
take n (x:xs) = x : take (n-1) xs

