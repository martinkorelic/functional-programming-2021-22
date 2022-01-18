module Exercise7 where

import Data.List

numbers :: [Int]
numbers = [11,10,5,10,6,5,1,3,8,2,2]

compactNums :: IO ()
compactNums = do
    let snum = nub $ sort numbers
    putStr $ compNum snum

compNum :: [Int] -> String
compNum ns = compNum' ns "" []

compNum' :: (Show a, Eq a, Enum a) => [a] -> [Char] -> [a] -> [Char]
compNum' [] str ls = str++(if length ls == 1 then show (head ls) else (show (head ls) ++ "-" ++ show (last ls)) ++ "\n")
compNum' (x:xs) str ls
    | null ls = compNum' xs str [x]
    | x == succ (last ls) = compNum' xs str (ls++[x])
    | otherwise = compNum' xs (str++(if length ls == 1 then show (head ls) else (show (head ls) ++ "-" ++ show (last ls))) ++ "\n") [x]