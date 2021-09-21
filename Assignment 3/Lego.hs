module Lego where

import Data.List
import Data.Tuple

removeAt :: Int -> [a] -> [a]
removeAt n xs = [ x | (i,x) <- zip [1..(length xs)] xs, i /= n]

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos xs = sort [ (x,i) | (i,x) <- zip [0..(length xs)] xs ]

addIndexes :: [(a,Int)] -> [(Int, a, Int)]
addIndexes xs = [(b,a,i) | ((a,b), i) <- zip xs [0..(length xs)]]

sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos xs = [(a,i) | (b, a,i) <- sort (addIndexes (sortWithPos xs))]