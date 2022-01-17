module Exercise1 where

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse a (x:xs) = x:a:intersperse a xs

allEqual :: Eq a => [a] -> Bool
allEqual ls = all (== head ls) ls

interspersed :: Eq a => [a] -> Bool
interspersed ls = odd (length ls) && allEqual [ x | (x,i) <- zip ls [1..], even i ]