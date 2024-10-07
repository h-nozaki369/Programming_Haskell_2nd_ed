instance Functor ((->) a) where
    -- fmap :: (b -> c) -> ((->) a) b -> ((->) a) c
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap = (.)

instance Applicative ((->) a) where
    -- pure :: b -> ((->) a) b
    -- pure :: b -> (a -> b)
    pure = const -- \x _ -> x
    -- <*> :: ((->) a) (b -> c) -> ((->) a) b -> ((->) a) c
    -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
    g <*> h = \x -> g x (h x)
