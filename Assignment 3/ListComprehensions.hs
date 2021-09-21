module ListComprehensions where

import Data.List

-- Cartesian product of as & bs
-- Fully polymorphic
g0 :: [a] -> [b] -> [(a, b)]
g0 as bs = [ (a,b) | a <- as, b <- bs ]

-- Repeats number y n-times in a list
-- Overloaded with a type class
g1 :: (Num t, Enum t) => t -> a -> [a]
g1 n y   = [ y | i <- [1..n] ]

-- Copies the list xs with the first n-items
-- Overloaded with a type class
g2 :: (Num a1, Enum a1, Ord a1) => a1 -> [a2] -> [a2]
g2 n xs  = [ x | (i,x) <- zip [0..] xs, i < n ]

-- Creates a list of index numbers of elements which are equal to a
-- Overloaded with a type class
g3 :: (Num a1, Enum a1, Eq a2) => a2 -> [a2] -> [a1]
g3 a xs  = [ i | (i,x) <- zip [0..] xs, x == a]

-- Zips two lists
-- Fully polymorphic
g4 :: [a] -> [a] -> [a]
g4 xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

-- Concatenates list of lists into a single list
-- Fully polymorphic
g5 :: [[a]] -> [a]
g5 xss   = [ x | xs <- xss, x <- xs ]