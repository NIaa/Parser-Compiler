import Prelude hiding (fail)

-- Syntax
type State = Int
data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr | Get | Put Expr Expr

-- Semantics
eval :: Expr -> State -> (Maybe Int, State)
eval (Val n) q = (Just n, q)
eval (Add x y) q = case eval x q of
                     (Just n, q') -> case eval y q' of
                                       (Just m, q'') -> (Just (n + m), q'')
                                       (Nothing, q'') -> (Nothing, q'')
                     (Nothing, q') -> (Nothing, q')
eval Throw q = (Nothing, q)           -- global state
eval (Catch x h) q = case eval x q of
                       (Just n, q')  -> (Just n, q')    -- local state
                       (Nothing, q') -> eval h q'
eval Get q = (Just q, q)
eval (Put x y) q = case eval x q of
                     (Just n, q')  -> eval y n
                     (Nothing, q') -> (Nothing, q')

-- Target LANG
data Code = HALT | PUSH Int Code | FAIL | ADD Code | MARK Code Code | UNMARK Code | LOAD Code | SAVE Code

-- Compiler
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' Throw c = FAIL
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Catch x h) c = MARK (comp' h c) (comp' x (UNMARK c))
comp' Get c = LOAD c
comp' (Put x y) c = comp' x (SAVE (comp' y c))

comp :: Expr -> Code
comp x = comp' x HALT

-- VM
type Stack = [Elem]
data Elem = VAL Int | HAN Code

type Conf = (Stack, State) -- configuration
exec :: Code -> Conf -> Conf
exec HALT (s, q) = (s, q)
exec (PUSH n c) (s, q) = exec c (VAL n : s, q)
exec (ADD c) (VAL m : VAL n : s, q) = exec c (VAL (n + m) : s, q)
exec FAIL (s, q) = fail (s, q)
exec (MARK h c) (s, q) = exec c (HAN h : s, q)
exec (UNMARK c) (VAL n : HAN _ : s, q) = exec c (VAL n : s, q)
exec (LOAD c) (s, q) = exec c (VAL q : s, q)
exec (SAVE c) (VAL n : s, q) = exec c (s, n)

fail :: Conf -> Conf
fail ([], q) = ([], q)
fail (VAL n : s, q) = fail (s, q)
fail (HAN h : s, q) = exec h (s, q)