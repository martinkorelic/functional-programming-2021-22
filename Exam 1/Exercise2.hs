module Exercise2 where

f1 :: ((a -> a) -> a) -> a
f1 f = f id

f2 :: Either a b -> (a -> b) -> b
f2 (Right b) _ = b
f2 (Left a) f = f a

f3 :: Monad m => m a -> m b -> m (a,b)
f3 m1 m2 = (,) <$> m1 <*> m2

f4 :: (a -> c, b -> c -> d) -> (a,b) -> d
f4 (z1, z2) (t1, t2) = z2 t2 (z1 t1) 