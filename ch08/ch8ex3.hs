data Tree a = Leaf a | Node (Tree a) (Tree a)

numLeaf :: Tree a -> Int
numLeaf (Leaf _) = 1
numLeaf (Node left right) = numLeaf left + numLeaf right

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left right) = abs (numLeaf left - numLeaf right) <= 1
                             && balanced left && balanced right

t :: Tree Int
t = Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 3)
