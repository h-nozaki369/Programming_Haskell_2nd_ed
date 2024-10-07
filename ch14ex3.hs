instance Foldable Maybe a where
    -- fold :: Monoid a => Maybe a -> a
    fold Nothing  = mempty
    fold (Just x) = x

    -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
    foldMap _ Nothing  = mempty
    foldMap g (Just x) = g x

    -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
    foldr _ y Nothing  = y
    foldr g y (Just x) = g x y

    -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
    foldl _ x Nothing  = x
    foldl g x (Just y) = g x y

instance Traversable Maybe a where
    -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse _ Nothing = pure Nothing
    traverse g (Just x) = Just <$> g x
