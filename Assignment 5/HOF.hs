module HOF where

import Prelude hiding (const)

{- exercise 5.1 -}

const :: p1 -> p2 -> p1
const x _y   = x

($->) :: t1 -> (t1 -> t2) -> t2
x $-> y      = y x

oper :: Fractional a => [Char] -> a -> a -> a
oper "mul" n = (*n)
oper "div" n = (n/)
oper _     _ = error "not implemented"

mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap f xs  = map (map f) xs

without :: (a -> Bool) -> [a] -> [a]
without p    = filter (not . p)

on :: (t1 -> t1 -> t2) -> (t3 -> t1) -> t3 -> t3 -> t2
on f g x y   = f (g x) (g y)

{- exercise 5.2 -}

f1 :: (Num a) => a -> a
f1 = (* 5) . (+ 1)
{- increment and then multiply by 5 -}

f2 :: (Num a) => a -> a
f2 = (+ 1) . (* 5)
{- multiply by 5 and increment by 1 -}

f3 :: (Num a, Ord a) => a -> a
f3 = (min 100) . (max 0)
{- maximum between 0 and a and then minimum between 100 and result of that -}

f4 :: [a] -> Bool
f4 = (<5) . length
{- length of the list and then if that length is less than 5 -}