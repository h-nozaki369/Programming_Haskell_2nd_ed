data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Foldable Tree a where
    -- fold :: Monoid a => Tree a -> a
    fold Leaf         = mempty
    fold (Node l x r) = fold l `mappend` x `mappend` fold r

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap _ Leaf         = mempty
    foldMap g (Node l x r) = foldMap g l `mappend` g x `mappend` foldMap g r

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr g y Leaf         = y
    foldr g y (Node l x r) = foldr g y l `mappend` g x `mappend` foldr g y r

    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl g x Leaf         = x
    foldl g x (Node l y r) = foldl g x l `mappend` g y `mappend` foldl g x r
