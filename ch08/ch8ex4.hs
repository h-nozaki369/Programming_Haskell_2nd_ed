data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

numLeaf :: Tree a -> Int
numLeaf (Leaf _) = 1
numLeaf (Node left right) = numLeaf left + numLeaf right

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left right) = abs (numLeaf left - numLeaf right) <= 1
                             && balanced left && balanced right

divList :: [a] -> ([a], [a])
divList xs = (take n xs, drop n xs) where n = length xs `div` 2

balance :: [a] -> Tree a
balance [] = error "list shall not be empty"
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs) where (ys, zs) = divList xs
