module AST where

--start with either:
data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Div Expr Expr -- | VarX
--or:
--  data Expr = Lit Integer | Expr :+: Expr | Expr :*: Expr | ...
--  infixl 6 :+:
--  infixl 7 :*:



eval :: (Fractional a) => Expr -> a
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Sub a b) = eval a - eval b
eval (Div a b) = eval a / eval b

--eval :: (Fractional a) => Expr -> a -> Maybe a