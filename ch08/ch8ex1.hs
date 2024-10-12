data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero        n = Zero
mult (Succ Zero) n = n
mult (Succ m)    n = add n (mult m n)
