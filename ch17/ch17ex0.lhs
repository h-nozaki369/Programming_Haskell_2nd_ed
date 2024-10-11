> data Expr = Val Int | Add Expr Expr
>
> eval :: Expr -> Int
> eval (Val n)   = n
> eval (Add x y) = eval x + eval y
>
> type Stack = [Int]
>
> eval' :: Expr -> Stack -> Stack

eval' must satisfy following property.
  eval' e s = eval e : s

-- base case
  eval' (Val n) s
=    { definition of eval' }
  eval (Val n) : s
=    { apply eval }
  n : s
=    { define as push n s = n : s }
  push n s

> eval' (Val n) = push n s

-- inductive case
  eval' (Add x y) s
=    { definition of eval' }
  eval (Add x y) : s
=    { apply eval }
  (eval x + eval y) : s
=    { define as add (m : n : s) = n+m : s }
  add (eval y : eval x : s)
=    { assumption of x }
  add (eval y : eval' x s)
=    { assumption of y }
  add (eval' y (eval' x s))

> eval' (Add x y) = add (eval' y (eval' x s))
>
> push :: Int -> Stack -> Stack
> push n s = n : s
>
> add :: Stack -> Stack
> add (m : n : s) = n+m : s

> eval :: Expr -> Int
> eval e = head (eval' e [])

For example,
  eval (Add (Val 1) (Val 2))
=    { apply eval}
  head (eval' (Add (Val 1) (Val 2)) [])
=    { apply eval' }
  head (add (eval' (Val 2) (eval' (Val 1) [])))
=    { apply inner eval' }
  head (add (eval' (Val 2) (push 1 [])))
=    { apply eval' }
  head (add (push 2 (push 1 [])))
=    { apply push }
  head (add (2 : 1 : [])))
=    { apply add }
  head (3 : [])
=    { apply head }
  3

Next, add continuation.

> type Cont = Stack -> Stack

and find out following function eval''.

  eval'' e c s = c (eval' e s)

-- base case
  eval'' (Val n) c s
=    { definition of eval'' }
  c (eval' (Val n) s)
=    { apply eval' }
  c (push n s)

-- inductive case
  eval'' (Add x y) c s
=    { definition of eval'' }
  c (eval' (Add x y) s)
=    { apply eval' }
  c (add (eval' y (eval' x s)))
=    { unapply . }
  (c . add) (eval' y (eval' x s))
=    { assumption of y }
  eval'' y (c . add) (eval' x s)
=    { assumption of x }
  eval'' x (eval'' y (c . add)) s

Therefore:
> eval'' :: Expr -> Cont -> Cont
> eval'' (Val n)   c s = c (push n s)
> eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s

Replacing c in eval'' with id gets eval'.
  eval' e s = eval'' e id s

Example evaluating 1+2:
  eval' (Add (Val 1) (Val 2)) []
=    { apply eval' }
  eval'' (Add (Val 1) (Val 2)) id []
=    { apply eval'' }
  eval'' (Val 1) (eval'' (Val 2) (id . add)) []
=    { apply outer eval'' }
  eval'' (Val 2) (id . add) (push 1 [])
=    { apply eval'' }
  (id . add) (push 2 (push 1 []))
=    { apply . }
  id (add (push 2 (push 1 [])))
=    { apply push }
  id (add (2 : 1 : []))
=    { apply add }
  id [3]
=    { apply id }
  [3]

As the third step, we will define combinators which consturct the continuation.
In the eval' and eval'', we use three continuations.
These are stop evaluation, push stack, and add two values on stack.

> haltC :: Int -> Cont -> Cont
> haltC = id
>
> pushC :: Int -> Cont -> Cont
> pushC n c = c . push n
>
> addC :: Cont -> Cont
> addC c = c . add

So, we can rewrite eval' and eval'' as below.

> eval' :: Expr -> Cont
> eval' e = eval'' e haltC
>
> eval'' :: Expr -> Cont -> Cont
> eval'' (Val n)   c = pushC n c
> eval'' (Add x y) c = eval'' x (eval'' y (addC c))

> data Code = HALT | PUSH Int Code | ADD Code deriving Show

> exec' :: Code -> Cont
> exec' HALT       = haltC
> exec' (PUSH n c) = pushC n (exec' c)
> exec' (ADD c)    = addC (exec' c)

-- case of HALT
  exec' HALT s
=    { apply exec' }
  haltC s
=    { apply haltC }
  id s
=    { apply id }
  s

-- case of PUSH
  exec' (PUSH n c) s
=    { apply exec' }
  pushC n (exec' c)
=    { apply pushC }
  (exec' c . push n) s
=    { apply . }
  exec' c (push n s)
=    { apply push }
  exec' c (n : s)

-- case of ADD
  exec' (ADD c) s
=    { apply exec' }
  addC (exec' c) s
=    { apply addC }
  (exec' c . add) s
=    { apply . }
  exec' c (add s)
=    { replace s with m:n:s' }
  exec' c (add (m : n : s'))
=    { apply add }
  exec' c (n+m : s')

Therefore we get following function exec.
> exec :: Code -> Stack -> Stack
> exec HALT       s           = s
> exec (PUSH n c) s           = exec c (n : s)
> exec (ADD c)    (m : n : s) = exec c (n+m : s)

Change name eval' and eval'' to comp and comp'.
Also replace combinators haltC, pushC, and AddC to constructers HALT, PUSH, and ADD.

> comp :: Expr -> Code
> comp e = comp' e HALT
>
> comp' :: Expr -> Code -> Code
> comp' (Val n)    c = PUSH n c
> comp' (Add x y) c = comp' x (comp' y (ADD c))
