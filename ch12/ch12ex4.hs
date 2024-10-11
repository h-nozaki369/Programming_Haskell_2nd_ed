newtype ZipList' a = Z [a] deriving Show

instance Functor ZipList' where
    -- fmap :: (a -> b) -> ZipList' a -> ZipList' b
    fmap f (Z xs) = Z (fmap f xs)

instance Applicative ZipList' where
    -- pure :: a -> ZipList' a
    pure x = Z (repeat x)

    -- (<*>) :: ZipList' (a -> b) -> ZipList' a -> ZipList' b 
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- (zip gs xs)]
