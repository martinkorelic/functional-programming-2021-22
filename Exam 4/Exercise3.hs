
type Nat = Int

data Expr
    = X
    | Lit Integer
    | Expr :+: Expr
    | Expr :*: Expr
    | Expr :^: Nat
    deriving (Eq, Show)

evalExpr :: (Monad m) => Expr -> m Integer
--evalExpr X = return 0

evalExpr (Lit i) = return i

evalExpr (e1 :+: e2) = evalExpr e1 >>= \t1 -> evalExpr e2 >>= \t2 -> return (t1 + t2)

evalExpr (e1 :*: e2) = evalExpr e1 >>= \t1 -> evalExpr e2 >>= \t2 -> return (t1 * t2)

evalExpr (e1 :^: e2) = evalExpr e1 >>= \t1 -> return (t1 ^ e2)

newtype M a = M { paramX :: Integer -> a }

instance Functor M where
    fmap f (M g) = M $ \env -> f (g env)
    
instance Monad M where
    return x = M . const
    (M m) >>= f = M $ \env -> paramX (f (m env)) env 

ask :: M Integer
ask = M $ id

evalExpr X = ask

type Term = (Integer, Nat)
type Poly = [Term]

evalPoly :: Poly -> Integer -> Integer
evalPoly [] n = 0
evalPoly ((c,i):xs) n = c * (n^i) + evalPoly xs n

polyPlus :: Poly -> Poly -> Poly
polyPlus [] p2 = p2
polyPlus p1 [] = p1
polyPlus p1@((f1,e1):ts1) p2@((f2,e2):ts2)
    | e1 == e2 = (f1+f2, e1) : polyPlus ts1 ts2
    | e1 < e2 = (f1,e1) : polyPlus ts1 p2
    | otherwise = (f2,e2) : polyPlus p1 ts2

termTimesPoly :: Term -> Poly -> Poly
termTimesPoly (f1,e1) = map (\(f2,e2) -> (f1*f2,e1+e2))

polyMul :: Poly -> Poly -> Poly
polyMul (p1:ps1) p2 = foldr polyPlus [] (map (flip termMul p2) p1)

polyPow :: Poly -> Nat -> Poly
polyPow p 0 = p
polyPow p n = polyMul p (polyPow p)

polyToExpr :: Poly -> Expr
polyToExpr ts = foldr (:+:) (Lit 0) (map (\(c,e) -> Lit c :*: (X :^: e)) ts)

exprToPoly :: Expr -> Poly
exprToPoly X = [(1,1)]
exprToPoly (Lit i) = [(i,0)]
exprToPoly (e1 :+: e2) = exprToPoly e1 `polyPlus` exprToPoly e2
exprToPoly (e1 :*: e2) = exprToPoly e1 `polyMul` exprToPoly e2
exprToPoly (e :^: n) = polyPow (exprToPoly e) n