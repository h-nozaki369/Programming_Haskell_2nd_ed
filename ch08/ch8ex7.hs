instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Nothing == _       = False
    _       == Nothing = False
    Just x  == Just y  = x == y

instance Eq a => Eq [a] where
    [] == [] = True
    [] == _  = False
    _  == [] = False
    (x:xs) == (y:ys) = x == y && xs == ys
