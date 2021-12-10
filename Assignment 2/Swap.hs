module Swap where

swap :: (a, b) -> (b, a)
swap (a,b) = (b,a)

-- swap1 :: (a,b) -> (b,a), does not work since of equality types
swap1 :: (Int, Int) -> (Int, Int)
swap1 (a,b) = if a == b then (b,a) else (b,a)

-- Does work since the types are polymorphic
swap2 :: (a, b) -> (b, a)
swap2 (a,b) = (const b a, const a b)

-- swap: (b,a) -> (a,b)
-- swap1: Eq b => (b,b) -> (b,b)
-- swap2: (b1,b2) -> (b2,b1)

-- Nested double tuple and triple tuple
conv :: (a,(b,c)) -> (a,b,c)
conv (a,(b,c)) = (a,b,c)