-- prove numbers of Leaf is always numbers of Node + 1 for
-- data Tree = Leaf Int | Node Tree Tree

> data Tree = Leaf Int | Node Tree Tree deriving Show

> leaves :: Tree -> Int
> leaves Leaf = 1
> leaves (Node l r) = leaves l + leaves r

> nodes :: Tree -> Int
> nodes Leaf = 0
> nodes (Node l r) = 1 + nodes l + nodes r

-- base case
-- prove leaves Leaf = nodes Leaf + 1
leaves Leaf = 1
    -- definition of leaves
nodes Leaf = 0
    -- definition of nodes
-- therefore leaves Leaf = nodes Leaf + 1 
--
-- inductive case
-- prove leaves (Node l r) = nodes (Node l r) + 1
-- with assumption leaves l = nodes l + 1 && leaves r = nodes r + 1
leaves (Node l r)
    -- definition of leaves
= leaves l + leaves r
    -- assumption
= (nodes l + 1) + (nodes r + 1)
    -- assosiative rule of +
= (nodes l + nodes r + 1) + 1
    -- unapply nodes
= nodes (Node l r) + 1
