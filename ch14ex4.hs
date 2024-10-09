import Data.Foldable

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g Leaf       = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

{-

Following Applicative and Monad instances of "Tree a" are just my excersises.
These are just examples of implementations. We should have other (and maybe better) implementations.

-}

instance Applicative Tree where
    -- pure :: a -> Tree a
    pure x = Node Leaf x Leaf
    
    -- (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    Node _ g _ <*> ty = fmap g ty

instance Monad Tree where
    -- (>>=) :: Tree a -> (a -> Tree b) -> Tree b
    Leaf >>= _ = Leaf
    (Node l x r) >>= g = case g x of
                             Leaf -> Leaf
                             Node _ y _ -> Node (l >>= g) y (r >>= g)

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold Leaf         = mempty
    fold (Node l x r) = (fold l) `mappend` x `mappend` (fold r)

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap _ Leaf         = mempty
    foldMap g (Node l x r) = foldMap g l `mappend` g x `mappend` foldMap g r

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr g y Leaf         = y
    foldr g y (Node l x r) = foldr g (g x (foldr g y r)) l

    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl g x Leaf         = x
    foldl g x (Node l y r) = foldl g (g (foldl g x l) y) r

instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g Leaf         = pure Leaf
    traverse g (Node l x r) = Node <$> traverse g l <*> g x <*> traverse g r
