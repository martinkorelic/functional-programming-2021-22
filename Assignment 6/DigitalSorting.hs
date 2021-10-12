module DigitalSorting where

import Data.List
import Data.Bool
import Data.Maybe
import Data.Either

class Rankable key where
  rank :: [(key,a)] -> [[a]]

digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

--genericRank :: (Ord key) => [(key,a)] -> [[a]]

--instance Rankable Int where ... etc.

--instance Rankable Bool where ...

--instance (Rankable key1, Rankable key2) => Rankable (key1,key2) where

--etc.
