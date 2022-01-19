g1 :: (a,b) -> ((b,a), (a,b))
g1 (a,b) = ((b,a), (a,b))

g2 :: (Either a () -> a) -> a
g2 f = f (Right ())

g3 :: (((Bool -> a) -> a) -> a) -> a
g3 f = f (\g -> g True)

g4 :: (Functor f) => f (Maybe a) -> f (Maybe (a,a))
g4 m = fmap (fmap (\a -> (a,a))) m

g5 :: (Functor f, Functor g) => g Char -> f String -> f (g String)
g5 gc fs = fmap (\s -> fmap (\c -> c:s) gc) fs

g6 :: (Monad m) => m (m ())
g6 = return (return ())

g7 :: (Applicative f) => f a -> f b -> f (b,a)
g7 f1 f2 = pure (flip (,)) <*> f1 <*> f2 

g8 :: (Monad m) => m (m (m a)) -> m a
g8 m1 = m1 >>= \m2 -> m2 >>= id