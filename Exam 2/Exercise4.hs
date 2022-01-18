module Exercise4 where
import Data.Tuple (swap)
import Control.Monad (liftM)

g1 :: (Functor f) => f (a, b) -> f (b, a)
g1 = fmap swap

g2 :: (Functor f) => f (f (a,b)) -> f (f (a, b, b, a))
g2 = fmap (fmap (\(a,b) -> (a,b,b,a)))

g3 :: (Functor f, Functor g) => f a -> g b -> f (g (a,b))
g3 m1 m2 = fmap (\x -> fmap (\y -> (x,y)) m2) m1

g4 :: (Applicative f, Applicative g) => a -> f (g a)
g4 a = pure (pure a)

g5 :: (Monad m) => a -> m (m a)
g5 a = return (return a)

g6 :: (Monad m) => m (m a) -> m a
g6 m = m >>= id