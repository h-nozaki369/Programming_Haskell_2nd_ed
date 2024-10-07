type State = Int

newtype ST' a = S (State -> (a, State))

instance Functor ST' where
    -- fmap :: (a -> b) -> ST' a -> ST' b
    --
    -- We need to get value of a from st :: ST' a. So, use >>=.
    --     st >>= (\x -> ...)
    -- Then apply g to x, and return it.
    --     st >>= (\x -> return (f x))
    fmap g st = do x <- st
                   return (f x)

instance Applicative ST' where
    -- pure :: a -> ST' a
    pure x = S (\s -> (x,s))

    -- (<*>) :: ST' (a -> b) -> ST' a -> ST' b
    --
    -- We need to get function from stf :: ST' (a -> b).
    --     stf >>= (\f -> ...)
    -- We also need to get value of a from stx :: ST' a.
    --     stf >>= (\f -> stx >>= (\x -> ...))
    --  Then apply f to x, and return it.
    --     stf >>= (\f -> stx >>= (\x -> return (f x)))
    stf <*> stx = do f <- stf
                     x <- stx
                     return (f x)

instance Monad ST' where
    -- (>>=) :: ST' a -> (a -> ST' b) -> ST' b
    st >>= f = S (\s ->
               let (x,s') = app st s in app (f x) s')

app :: ST' a -> State -> (a, State)
app (S st) x = st x
