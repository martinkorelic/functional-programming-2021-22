module Exercise5 where

data M a = M (IO a)

instance Functor M where
    fmap f (M io) = M (io >>= \r -> return (f r))

instance Applicative M where
    pure io = M $ return io
    (<*>) (M f) (M io) = M $ (f >>= \f -> io >>= \a -> return (f a))