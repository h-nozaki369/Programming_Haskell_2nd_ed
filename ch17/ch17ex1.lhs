It is not completed yet.

We have following components.
  Type for Syntax of Source Language: Expr
  Function for Semantics of Source Language: eval :: Expr -> Int
  Type for Int stack: Stack
  Type for Virtual Machine Code: Code
  Compiler function: comp :: Expr -> Code
  Helper function: comp' :: Expr -> Code -> Code
  Function executing Machine Code: exec :: Code -> Stack -> Stack

> data Expr = Val Int
>           | Add Expr Expr
>           | Throw
>           | Catch Expr Expr
>           deriving Show

> eval :: Expr -> Maybe Int
> eval (Val n)     = Just n
> eval (Add x y)   = case eval x of
>                        Just n -> case eval y of
>                            Just m  -> Just (n + m)
>                            Nothing -> Nothing
>                        Nothing -> Nothing
> eval Throw       = Nothing
> eval (Catch x h) = case eval x of
>                        Just n  -> Just n
>                        Nothing -> eval h
>
> type Stack = [Int]
>
> data Code = undefined

eval, comp, and exec have following relationship.
  exec (comp e) s = eval e : s
  exec (comp' e c) s = exec c (eval e : s)

We will transform the equation of comp' to a form of:
  exec (comp' e c) s = exec c' s

Base case of Val n:
  exec (comp' (Val n) c) s
=    { property of comp' }
  exec c (eval (Val n) : s)
=    { apply eval }
  exec c (Just n : s)

c and n are variable. So, we need a Code which takes those variables.
We define it as PUSH :: Int -> Code -> Code.
The function exec needs following.
  exec (PUSH n c) s = exec c (n : c)

So that,
  exec c (Just n : s)
=    { unapply exec }
  exec (PUSH n c) s

We can coclude:
  comp' (Val n) c = PUSH n c

Inductive case of Add x y:
  exec (comp' (Add x y) c) s
=    { property of comp' }
  exec c (eval (Add x y) : s)
=    { apply eval }
  exec c (case eval x of
              Just n  -> case eval y of
                  Just m  -> Just (n + m)
                  Nothing -> Nothing
              Nothing -> Nothing)

We have following assumptions.
  exec (comp' x c') s' = exec c' (eval x : s')
  exec (comp' y c') s' = exec c' (eval y : s')
