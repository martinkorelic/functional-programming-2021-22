module AST where

-- this template uses infix constructors; feel free to use AST.hs (which uses infix ones) if you prefer
-- (if you really liked your own solution to Exercise 4.7, you can use that as well)

import Result

type Identifier = String

data Expr = Lit Integer | Var Identifier | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving (Show)

eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Result a

eval (Lit k) _ = Okay (fromInteger k) 
eval (Add x y) vars = (+) <$> eval x vars <*> eval y vars
  
  --case (eval x vars, eval y vars) of 
  --                   (Just x', Just y') -> Just (x'+y')
  --                   _ -> Nothing
                       
eval (Sub x y) vars = (-) <$> eval x vars <*> eval y vars
  --case (eval x vars, eval y vars) of 
  --                   (Just x', Just y') -> Just (x'-y')
  --                   _ -> Nothing

eval (Mul x y) vars = (*) <$> eval x vars <*> eval y vars
  
  --case (eval x vars, eval y vars) of
  --                   (Just x', Just y') -> Just (x'*y') 
  --                   _ -> Nothing 

eval (Div x y) vars = case (eval x vars, eval y vars) of 
                     (Okay x', Okay 0)  -> Error ["division by zero"]
                     (Error x', Okay 0) -> Error x' <*> Error ["division by zero"]
                     (Okay x', Okay y') -> (/) <$> pure x' <*> pure y'
                     (Error x', Okay y') -> Error x'
                     (Okay x', Error y') -> Error y'
                     (Error x', Error y') -> Error x' <*> Error y'

eval (Var name) [] = Error ["unknown variable: " ++ name]
eval (Var name) (v:vars) = if name == fst v then Okay (snd v) else eval (Var name) vars