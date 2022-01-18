module Exercise1 where
import Data.List (nub)

data Formula var = Var var | Not (Formula var) | And (Formula var) (Formula var) | Or (Formula var) (Formula var)

type Environment var val = [(var,val)]

lookup' :: (Eq var) => var -> Environment var val -> Maybe val
lookup' var [] = Nothing
lookup' var ((var',val) : env)
  | var == var'  = Just val
  | otherwise    = lookup' var env

eval :: (Eq var) => Environment var Bool -> Formula var -> Maybe Bool
eval env (Var var) = lookup' var env
eval env (Not f1) = fmap not (eval env f1)
eval env (And f1 f2) =  eval env f1 >>= \v1 ->
                        eval env f2 >>= \v2 ->
                        return (v1 && v2)
eval env (Or f1 f2) =   eval env f1 >>= \v1 ->
                        eval env f2 >>= \v2 ->
                        return (v1 || v2)

all_valuations :: [var] -> [val] -> [Environment var val]
all_valuations [] _ = [[]]
all_valuations (var : vars) vals = concat [ concat [ [((var,val):valuation)] | val <- vals] | valuation <- all_valuations vars vals]

vars :: (Eq var) => Formula var -> [var]
vars (Var var) = [var]
vars (Not f1) = vars f1
vars (And f1 f2) = nub (vars f1 ++ vars f2)
vars (Or f1 f2)  = nub (vars f1 ++ vars f2)

is_true (Just True) = True
is_true _ = False

truths :: (Eq var) => Formula var -> [Environment var Bool]
truths f = [env | env <- all_valuations (vars f) [True,False], is_true (eval env f)]