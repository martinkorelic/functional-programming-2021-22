module ListFunctions where

import Prelude hiding (or, and, elem, take, drop)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) = a == x || elem a xs 

drop :: Int -> [a] -> [a]
drop 0 d = d
drop i [] = []
drop i (x:xs) = drop (i-1) xs

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take i (x:xs) = x : take (i-1) xs