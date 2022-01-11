module Pronounceable where

import Data.List
import Say (say)

class Pronounceable a where
  pronounce :: a -> String

instance Pronounceable Char where
  pronounce c = unwords ["the","character", "'"++[c]++"'"]

instance Pronounceable a => Pronounceable [a] where
  pronounce xs = "a list containing " ++ (intercalate ", " $ map pronounce xs)

instance Pronounceable Integer where
  pronounce c = say c

instance Pronounceable Int where
  pronounce c = say (toInteger c)

instance Pronounceable Double where
  pronounce c = say z ++ " point " ++ say (truncate $ (fromIntegral z - c)*10)
    where z = truncate c

instance (Pronounceable a, Pronounceable b) => Pronounceable (a,b) where
  pronounce (a,b) = pronounce a ++ pronounce b