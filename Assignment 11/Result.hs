module Result where

import Data.List

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

--instance Functor Result where
--  ...

--instance Applicative Result where
--  ...
