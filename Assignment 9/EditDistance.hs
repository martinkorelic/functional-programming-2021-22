module EditDistance where

import Data.Array

naiveEditDistance :: String -> String -> Int
naiveEditDistance xs ys = distance xs ys
  where
  distance :: String -> String -> Int
  distance [] ys = length ys
  distance xs [] = length xs
  distance (x:xs) (y:ys) = minimum [1+distance xs (y:ys), 1+distance (x:xs) ys, cost x y+distance xs ys]

  cost x y = if x==y then 0 else 1


--editDistance :: String -> String -> Int
--editDistance xs ys = ...
--  where
--  ??? :: (Int,Int) -> Int
--
--  distArray :: Array (Int,Int) Int
--  distArray = ...
--
