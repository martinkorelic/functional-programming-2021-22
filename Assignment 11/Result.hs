module Result where

import Data.List

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

instance Functor Result where
  fmap func (Okay a) = Okay (func a)
  fmap func (Error a) = Error a

-- Type of fmap is
-- fmap :: (a -> b) -> Result a -> Result b

instance Applicative Result where
  pure = Okay
  Okay a <*> b = fmap a b
  Error a <*> Error b = Error (a ++ b)
  Error a <*> b = Error a

-- Type of pure is:
-- pure :: a -> Result a

-- Type of (<*>) is:
-- <*> :: Result (a -> b) -> Result a -> Result b