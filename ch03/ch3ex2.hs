bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[0,1], [1,2]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x
