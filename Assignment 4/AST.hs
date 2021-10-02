module AST where
import Distribution.Simple.Utils (xargs)

--start with either:
data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Div Expr Expr | VarX
--or:
--  data Expr = Lit Integer | Expr :+: Expr | Expr :*: Expr | ...
--  infixl 6 :+:
--  infixl 7 :*:

eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
eval (Lit a) _ = Just (fromInteger a)
eval VarX x = Just x
eval (Div a b) 0 = Nothing
eval (Mul a b) x = case (x1, x2) of 
                    (Just x1, Just x2) -> Just (x1 * x2)
                    (Nothing, Just x2) -> Nothing
                    (Just x1, Nothing) -> Nothing
                    (Nothing, Nothing) -> Nothing
                    where x1 = eval a x
                          x2 = eval b x
eval (Div a b) x = case (x1, x2) of 
                    (Just x1, Just x2) -> if x2 == 0 then Nothing else Just (x1 / x2)
                    (Nothing, Just x2) -> Nothing
                    (Just x1, Nothing) -> Nothing
                    (Nothing, Nothing) -> Nothing
                    where x1 = eval a x
                          x2 = eval b x
eval (Sub a b) x = case (x1, x2) of 
                    (Just x1, Just x2) -> Just (x1 - x2)
                    (Nothing, Just x2) -> Nothing
                    (Just x1, Nothing) -> Nothing
                    (Nothing, Nothing) -> Nothing
                    where x1 = eval a x
                          x2 = eval b x
eval (Add a b) x = case (x1, x2) of 
                    (Just x1, Just x2) -> Just (x1 + x2)
                    (Nothing, Just x2) -> Nothing
                    (Just x1, Nothing) -> Nothing
                    (Nothing, Nothing) -> Nothing
                    where x1 = eval a x
                          x2 = eval b x