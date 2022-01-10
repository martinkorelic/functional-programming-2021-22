module CollapsingLists where

concatr :: [[a]] -> [a]
concatr [] = []
concatr (x:xs) = x ++ concatr xs

concatl :: [[a]] -> [a]
concatl [] = []
concatl (x:xs) = concatl xs ++ x

{-

3.
concatr is more efficient as (++) has to traverse through the whole first argument. 

4.
No, as the order in which the lists mix is different.

-}