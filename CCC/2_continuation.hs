-- Syntax
data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr

-- Semantics
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) = case eval x of
                   Nothing -> Nothing
                   Just n -> case eval y of
                     Nothing -> Nothing
                     Just m -> Just (n + m)
eval Throw = Nothing
eval (Catch x h) = case eval x of
                     Nothing -> eval h
                     Just n  -> Just n


-- Target Lang
data Code = HALT | PUSH Int Code | ADD Code | POP Code

-- Compiler
comp' :: Expr -> Code -> Code -> Code
comp' (Val n) sc fc = PUSH n sc
comp' Throw sc fc = fc
comp' (Catch x h) sc fc = comp' x sc (comp' h sc fc)
comp' (Add x y) sc fc = comp' x (comp' y (ADD sc) (POP fc)) fc

comp :: Expr -> Code
comp x = comp' x HALT HALT

-- VM
type Stack = [Elem]
data Elem = VAL Int

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n sc) s = exec sc (VAL n : s)
exec (ADD sc) (VAL m : VAL n : s) = exec sc (VAL (n + m) : s)
exec (POP fc) (VAL _ : s) = exec fc s