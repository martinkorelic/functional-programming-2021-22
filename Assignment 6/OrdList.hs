module OrdList where

import Data.List

{-

mempty = []
(<>) = some sorting of list (asc,desc)

-}

newtype OrdList a = OrdList [a]
  deriving (Eq,Ord,Show)

instance (Ord a) => Semigroup (OrdList a) where
    (OrdList a) <> (OrdList b) = OrdList $ sort (a ++ b)
  
instance (Ord a) => Monoid (OrdList a) where
    mempty = OrdList []