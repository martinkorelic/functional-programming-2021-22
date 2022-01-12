module ListHOF where

import Data.List
import Data.Char
import Data.Function
import Data.Maybe

sortLength :: [String] -> [String]
sortLength = sortOn length 

letterClump :: String -> [String]
letterClump = groupBy (\x y -> isLetter x && isLetter y)

-- note: test this with 'take 20 fibs'
fibs :: [Integer]
fibs = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f a b = map (uncurry f) $ zip a b