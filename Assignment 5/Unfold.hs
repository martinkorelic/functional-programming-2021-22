module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))

bits :: Int -> [Int]
bits n = unfoldr (\s -> if s == 0 then Nothing else Just (s `mod` 2, s `div` 2)) n

zip :: [a] -> [b] -> [(a,b)]
zip x y = unfoldr (\s -> if length x > s && length y > s then Just ((x !! s, y !! s), s+1) else Nothing) 0

take :: Int -> [a] -> [a]
take n x = unfoldr (\s -> if length x > s && n > s then Just (x !! s, s+1) else Nothing) 0

primes :: [Integer]
primes = unfoldr (\s -> Just (s, nextPrime s 0)) 2

nextPrime :: Integer -> Integer -> Integer
nextPrime n f   | f /= 0 && length [x | x <- [2 .. n-1], mod n x == 0] == 0 = n
                | otherwise = nextPrime (n+1) (f+1)

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

(++) :: [a] -> [a] -> [a]
(++) a b = apo (\s -> if length a > s then Right (a !! s, s+1) else Left b) 0

insert :: (Ord a) => a -> [a] -> [a]
insert x ls = apo (\(s:ss) -> if s >= x then Right (s, ss) else Left (x:s:ss)) ls

unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
unfoldrApo step seed = apo (\s -> case step s of Just (a,t) -> Right (a,t); Nothing -> Left (unfoldr step seed)) seed