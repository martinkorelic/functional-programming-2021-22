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

--head :: Stream a -> a

--tail :: Stream a -> Stream a

--repeat :: a -> Stream a

--map :: (a -> b) -> (Stream a -> Stream b)

--zipWith :: (a -> b -> c) -> (Stream a -> Stream b -> Stream c)

--filter :: (a -> Bool) -> Stream a -> Stream a

--toList :: Stream a -> [a]

--cycle :: [a] -> Stream a

nat, fib :: Stream Integer
nat = 0 :> zipWith (+) nat (repeat 1)
fib = 0 :> 1 :> zipWith (+) fib (tail fib)

--primetwins :: Stream (Integer,Integer)

--combine :: Stream a -> Stream a -> Stream a
