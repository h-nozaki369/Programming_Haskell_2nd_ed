instance Monad (a ->) where
    -- return :: b -> (a ->) b
    -- return :: b -> (a -> b)
    return = const

    -- (>>=) :: (a ->) b -> (b -> ((a ->) c)) -> ((a ->) c)
    -- (>>=) :: (a -> b) -> (b -> a -> c) -> (a -> c)
    g >>= h = \x -> h (g x) x
