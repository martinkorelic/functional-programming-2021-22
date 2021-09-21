module Lego where

import Data.List
import Data.Tuple

removeAt :: Int -> [a] -> [a]
removeAt n xs = [ x | (i,x) <- zip [1..(length xs)] xs, i /= n]

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos xs = sort [ (x,i) | (i,x) <- zip [0..(length xs)] xs ]

-- findPairs xs = map () xs

-- TODO
sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos xs = [ (y,i) | (x,i) <- sortWithPos xs, y <- xs, x == y ]