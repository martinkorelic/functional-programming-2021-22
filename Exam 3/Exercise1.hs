module Exercise1 where

g1 :: (a, b, c, d) -> (b, c, d, a)
g1 (a, b, c, d) = (b, c, d, a)

g2 :: a -> (b -> (a, (b,b), a))
g2 a b = (a,(b,b),a)

g3 :: ((a,b,c) -> d) -> c -> b -> a -> d
g3 f c b a = f (a,b,c)

g4 :: (Int -> a) -> a
g4 f = f 0

g5 :: (((Int -> a) -> a) -> a) -> a
g5 f = f g4