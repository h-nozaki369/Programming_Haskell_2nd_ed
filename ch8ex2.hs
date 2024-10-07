data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node left y right) = case compare x y of
    EQ -> True
    LT -> occurs x left
    GT -> occurs x right
