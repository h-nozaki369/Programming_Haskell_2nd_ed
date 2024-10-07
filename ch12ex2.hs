instance Functor ((->) a) where
    -- fmap :: (b -> c) -> ((->) a) b -> ((->) a) c
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap = (.)
