module FunList where

compose :: [a -> a] -> (a -> a)
compose [] x = x
compose (f:fs) x = f (compose fs x)

compose' :: [a -> a] -> (a -> a)
compose' f x = foldr (\y -> y) x f

foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1

-- map (*) [1..n] - Partial functions with the first argument of multiplication being a member of the list from 1 to n.
-- compose takes the list of partial functions which are then folded right with the given argument of "1"
-- The last of function (n) is then applied the argument 1 and recursively returns it back
-- Final sequence: 1 * (2 * (3 * (...n)))
-- Multiplication of first n natural numbers.

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' step h fl = compose (map step fl) h