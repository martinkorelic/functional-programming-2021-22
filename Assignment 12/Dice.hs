module AST where

import System.Random
import RandomState

data Expr = Lit Integer | Dice Integer 
          | Expr :+: Expr
          | Min Expr Expr | Max Expr Expr
  deriving (Show)

infixl 6 :+: 

type DiceAction m = Integer -> m Integer

--evalM :: Expr -> DiceAction IO -> IO Integer             -- prototype
--evalM :: (Monad m) => Expr -> DiceAction m -> m Integer  -- final version

--evalRIO :: Expr -> IO Integer
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
--  where report x = do { putStr "rolled a "; print x; return x }

--evalIO :: Expr -> IO Integer

--evalND :: Expr -> [Integer]

avg :: (Fractional a) => [Integer] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

--expectation :: (Fractional a) => Expr -> a
--expectation e = avg (evalND e)

--evalR :: Expr -> RandomState Integer

--observed :: (Fractional a) => Int -> Expr -> IO a
