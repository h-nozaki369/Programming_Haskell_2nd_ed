data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> (Expr a) -> (Expr b)
    fmap g (Var x)   = Var (g x)
    fmap _ (Val x)   = Val x
    fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
    -- pure :: a -> (Expr a)
    pure x = Var x

    -- (<*>) :: Expr (a -> b) -> (Expr a) -> (Expr b)
    Val _   <*> _         = error "Val cannot be left term of <*>."
    _       <*> (Val x)   = Val x
    Var g   <*> (Var x)   = Var (g x)
    Var g   <*> (Add x y) = Add (fmap g x) (fmap g y)
    Add g h <*> x         = Add (g <*> x) (h <*> x)

instance Monado Expr where
    -- return :: a -> (Expr a)
    return = pure

    -- (>>=) :: (Expr a) -> (a -> Expr b) -> (Expr b)
    Var x   >>= g = g x
    Val x   >>= _ = Val x
    Add x y >>= g = Add (x >>= g) (y >>= g)
