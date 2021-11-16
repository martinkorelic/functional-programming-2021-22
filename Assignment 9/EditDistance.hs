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


editDistance :: String -> String -> Int
editDistance xs ys = distArray ! (max_x, max_y)
  where
  max_x = length xs
  max_y = length ys

  distance :: (Int,Int) -> Int
  distance (0, ys) = ys
  distance (xs, 0) = xs
  distance (xs, ys) = minimum [1 + distArray ! (xs-1, ys), 1 + distArray ! (xs, ys-1), cost xs ys + distArray ! (xs-1, ys-1)]
  cost x y = if (xs !! (x-1)) == (ys !! (y-1)) then 0 else 1

  distArray :: Array (Int,Int) Int
  distArray = array ((0,0), (max_x, max_y)) [(ij, distance ij) | i <- [0..max_x], j <- [0..max_y], let ij = (i,j)]