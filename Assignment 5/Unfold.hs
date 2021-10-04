module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))

-- bits :: Int -> [Int]
-- zip :: [a] -> [b] -> [(a,b)]
-- take :: Int -> [a] -> [a]
-- primes :: [Integer]

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

-- (++) :: [a] -> [a] -> [a]
-- insert :: (Ord a) => a -> [a] -> [a]
-- unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
