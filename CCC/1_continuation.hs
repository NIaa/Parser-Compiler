import Prelude hiding (fail)

-- Syntax
data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr

-- Semantics
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) = case eval x of
                   Just n -> case eval y of
                               Just m -> Just (m + n)
                               Nothing -> Nothing
                   Nothing -> Nothing
eval Throw = Nothing
eval (Catch x h) = case eval x of
                     Just n -> Just n
                     Nothing -> eval h

-- Target Lang
data Code = HALT | PUSH Int Code | ADD Code | FAIL | MARK Code Code | UNMARK Code

-- Compiler
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Throw) c = FAIL
comp' (Catch x h) c = MARK (comp' h c) (comp' x (UNMARK c))

comp :: Expr -> Code
comp x = comp' x HALT

-- VM
type Stack = [Elem]
data Elem = VAL Int | HAN Code

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (VAL n : s)
exec FAIL s = fail s
exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n+m) : s)
exec (MARK c' c) s = exec c (HAN c' : s)
exec (UNMARK c) (VAL n : HAN _ : s) = exec c (VAL n : s)

fail :: Stack -> Stack
fail [] = []
fail (VAL n:s) = fail s
fail (HAN c:s) = exec c s