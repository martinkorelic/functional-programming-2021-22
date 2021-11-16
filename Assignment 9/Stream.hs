module Stream where

import qualified Data.List as List
import Prelude hiding (head, tail, repeat, map, zipWith, filter, take, drop, concat, cycle, sum)

data Stream a = a :> Stream a
infixr 5 :>

instance (Show a) => Show (Stream a) where
  show s = "(" List.++ showN (16::Int) s List.++ ")"
    where
    showN 0 _         = "..."
    showN n (x :> xs) = show x List.++ " :> " List.++ showN (n-1) xs

from :: Integer -> Stream Integer
from n = n :> from (n + 1)

head :: Stream a -> a
head (a :> b) = a

tail :: Stream a -> Stream a
tail (a :> b) = b

repeat :: a -> Stream a
repeat a = a :> repeat a

map :: (a -> b) -> (Stream a -> Stream b)
map f (a :> b) = f a :> map f b

zipWith :: (a -> b -> c) -> (Stream a -> Stream b -> Stream c)
zipWith f (a :> b) (c :> d) = f a c :> zipWith f b d

filter :: (a -> Bool) -> Stream a -> Stream a
filter f (a :> b) = if f a then a :> filter f b else filter f b

--using filter (\x False) (from 0) will result in iterating through the stream but every
--element will be filtered out

toList :: Stream a -> [a]
toList (a :> b) = a : toList b

cycle :: [a] -> Stream a
cycle xs = foldr (:>) (cycle xs) xs

nat, fib :: Stream Integer
nat = 0 :> zipWith (+) nat (repeat 1)
fib = 0 :> 1 :> zipWith (+) fib (tail fib)

primetwins :: Stream (Integer,Integer)
primetwins = filter (\(a,b) -> a+2==b) (zipWith tuple (primes 2) (primes 3))

tuple :: a -> b -> (a,b)
tuple a b = (a,b)

primes :: Integer -> Stream Integer
primes n = filter (>=n) (cycle (sieve [2..]))
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

combine :: Stream a -> Stream a -> Stream a
combine (a :> b) (c :> d) = a :> c :> combine b d
