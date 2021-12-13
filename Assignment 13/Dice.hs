module Dice where

import Control.Monad
import System.Random
import RandomState
import RandomGen (genRandInteger)

data Expr = Lit Integer | Dice Integer 
          | Expr :+: Expr
          | Min Expr Expr | Max Expr Expr
          | Div Expr Integer | Expr :-: Expr
  deriving (Show)

infixl 6 :+:
infixl 7 :-:

type DiceAction m = Integer -> m Integer

--evalM :: Expr -> DiceAction IO -> IO Integer             -- prototype
evalM :: (Monad m) => Expr -> DiceAction m -> m Integer  -- final version
evalM (Lit i) dice = return i
evalM (Dice i) dice = dice i
evalM (Min x y) dice = do
  dx <- evalM x dice
  dy <- evalM y dice
  return $ min dx dy
evalM (Max x y) dice = do
  dx <- evalM x dice
  dy <- evalM y dice
  return $ max dx dy
evalM (Div x y) dice = do
  dx <- evalM x dice
  return $ dx `div` y
evalM (x :+: y) dice = do
  dx <- evalM x dice
  dy <- evalM y dice
  return $ dx + dy
evalM (x :-: y) dice = do
  dx <- evalM x dice
  dy <- evalM y dice
  return $ dx - dy

evalRIO :: Expr -> IO Integer
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version
evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
  where report x = do { putStr "rolled a "; print x; return x }

evalIO :: Expr -> IO Integer
evalIO expr = evalM expr askUser
  where askUser b = do { putStrLn $ "Please choose a number between 1.." ++ show b; answer <- getLine; 
                          let h = read answer in if h <= b then return h else do {putStrLn $ "Not in range."; askUser b} }

evalND :: Expr -> [Integer]
evalND (Lit i) = [i]
evalND (Dice i) = [1..i]
evalND (x :+: y) = case (length x1 == 1,length y1 == 1) of
                    (True, True) -> [head x1 + head y1]
                    (True, False) -> map (+ head x1) y1
                    (False, True) -> map (+ head y1) x1
                    _ -> [ z1+z2 | z1 <- x1, z2 <- y1]
                    where
                      x1 = evalND x
                      y1 = evalND y
evalND (x :-: y) = case (length x1 == 1,length y1 == 1) of
                    (True, True) -> [head x1 - head y1]
                    (True, False) -> map ((-) $ head x1) y1
                    (False, True) -> map ((-) $ head y1) x1
                    _ -> [ z1-z2 | z1 <- x1, z2 <- y1]
                    where
                      x1 = evalND x
                      y1 = evalND y
evalND (Div x c) = map (`div` c) $ evalND x
evalND _ = []

avg :: (Fractional a) => [Integer] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

expectation :: (Fractional a) => Expr -> a
expectation e = avg (evalND e)

evalR :: Expr -> RandomState Integer
evalR expr = evalM expr (\dice -> genRandInteger (1,dice))

observed :: (Fractional a) => Int -> Expr -> IO a
observed i e = do
  a <- replicateM i (evalRIO e)
  return $ avg a