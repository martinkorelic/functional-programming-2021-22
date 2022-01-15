module FindDefs where

import Control.Applicative

(?$) :: Maybe (a -> b) -> Maybe a -> Maybe b
(?$) f a2 = f <*> a2

product :: (Applicative t) => t a -> t b -> t (a,b)
product = liftA2 (,)

apply :: [a -> b] -> a -> [b]
apply fs a = map (\x -> x a) fs

apply2nd :: [a -> b -> c] -> b -> [a -> c]
apply2nd fs b = map (`flip` b) fs