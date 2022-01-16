{-# LANGUAGE InstanceSigs #-}
module TraverseExpr where

import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Maybe

data Expr var = Var var | Lit Integer | Op BinOp (Expr var) (Expr var)
  deriving (Show,Eq)
data BinOp    = Add | Sub | Mul | Div
  deriving (Show,Eq)

instance Functor Expr where
  fmap :: (a -> b) -> (Expr a -> Expr b)
  fmap f (Lit x) = Lit x
  fmap f (Var v) = Var (f v)
  fmap f (Op o x y) = Op o (fmap f x) (fmap f y)

instance Foldable Expr where
  foldMap :: (Monoid m) => (a -> m) -> Expr a -> m
  foldMap f (Lit x) = mempty
  foldMap f (Var v) = f v
  foldMap f (Op o x y) = foldMap f x <> foldMap f y

instance Traversable Expr where
  traverse :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)
  traverse f (Lit x) = pure (Lit x)
  traverse f (Var v) = Var <$> f v
  traverse f (Op o x y) = Op o <$> traverse f x <*> traverse f y 

{-
To fold an expression is to ignore/not apply function on Literals (Lit), apply the function
on the variables (Var) and to recursively fold on both sides/expressions of the binary operation, while
ignoring the binary operation itself.
-}

allVars :: (Ord a) => Expr a -> [a]
allVars = foldr (\x acc -> if x `elem` acc then acc else acc ++ [x]) []

renameVar :: String -> State [(String,Int)] Int
renameVar name = do 
  vars <- get
  let i = fromMaybe (-1) $ lookup name vars
  if i == -1 then do { put (vars ++ [(name, length vars)]); return $ length vars } else return i

indexVars :: Expr String -> Expr Int
indexVars ex = evalState (renameAllVars ex) []

renameAllVars :: Expr String -> State [(String, Int)] (Expr Int)
renameAllVars = traverse renameVar

-- Exercise 14.5

traverseVars :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)
traverseVars = traverse

traverseLits :: (Applicative f) => (Integer -> f Integer) -> Expr a -> f (Expr a)
traverseLits fi (Lit x) = Lit <$> fi x
traverseLits fi (Var v) = pure (Var v)
traverseLits fi (Op o x y) = Op o <$> traverseLits fi x <*> traverseLits fi y

traverseBinOps :: (Applicative f) => (BinOp -> f BinOp) -> Expr a -> f (Expr a)
traverseBinOps fi (Lit x) = pure (Lit x)
traverseBinOps fi (Var v) = pure (Var v)
traverseBinOps fi (Op o x y) = Op <$> fi o <*> traverseBinOps fi x <*> traverseBinOps fi y