module PolyTypes where

--lift0 :: (t1 -> t2) -> t1 -> t2
lift0 f x          = f x

--lift1 :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
lift1 f g1 x       = f (g1 x)
--lift2 :: (t1 -> t2 -> t3) -> (t4 -> t1) -> (t4 -> t2) -> t4 -> t3
lift2 f g1 g2 x    = f (g1 x) (g2 x)


--deMorgan :: ((a1 -> Bool) -> a2 -> Bool) -> (a1 -> Bool) -> a2 -> Bool
deMorgan quantor p   = not . quantor (not . p)
