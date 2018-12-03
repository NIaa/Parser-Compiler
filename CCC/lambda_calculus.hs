-- Syntax
data Expr = Val Int | Add Expr Expr | Var Int | Abs Expr | App Expr Expr

-- Semantics
data Value = Num Int | Clo Expr Env
type Env = [Value] -- for additional data structure

-- apply :: Lam -> (Value -> Value)
-- apply (Clo x e) = \v -> eval x (v:e)

eval :: Expr -> Env -> Value
eval (Val n) e = Num n
eval (Add x y) e = case eval x e of
                     Num n -> case eval y e of
                                Num m -> Num (n + m)
eval (Var i) e = e !! i                      -- de Bruijn index
eval (Abs x) e = Clo x e
eval (App x y) e = case eval x e of
                     Clo x' e' -> eval x' (eval y e : e')

-- Target Lang
data Code = HALT | PUSH Int Code | LOOKUP Int Code | ADD Code | ABS Code Code | RET | APP Code

-- Compiler
comp :: Expr -> Code
comp x = comp' x HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Var i) c = LOOKUP i c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Abs x) c = ABS (comp' x RET) c
comp' (App x y) c = comp' x (comp' y (APP c))

-- VM
data Elem = VAL Value' | CLO Code Env'

data Value' = Num' Int | Clo' Code Env'
type Env' =[Value']

type Conf = (Stack, Env')
type Stack = [Elem]

exec :: Code -> Conf -> Conf
exec (PUSH n c) (s, e) = exec c (VAL (Num' n) : s, e)
exec (LOOKUP i c) (s, e) = exec c (VAL (e!!i) : s, e)
exec (ADD c) (VAL (Num' m) : VAL (Num' n) : s, e) =  exec c (VAL (Num' (n+m)) : s, e)
exec (ABS c' c) (s, e) = exec c (VAL (Clo' c' e) : s, e)
exec RET (VAL v : CLO c e : s, _) = exec c (VAL v : s, e)
exec (APP c) (VAL v : VAL (Clo' c' e'):s, e) = exec c' (CLO c e : s, v : e')

-- Convertion
conv :: Value -> Value'
conv (Num n) = Num' n
conv (Clo x e) = Clo' (comp' x RET) (map conv e)