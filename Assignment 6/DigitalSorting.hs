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

genericRank :: (Ord key) => [(key,a)] -> [[a]]
genericRank = map (map snd . sortOn fst) . groupBy (\(x,i) (y,l) -> x == y) . sortOn fst

instance Rankable Int where
  rank = genericRank

instance Rankable Integer where
  rank = genericRank

instance Rankable Char where
  rank = genericRank

--instance Rankable Bool where
--  rank [] = []
--  rank (l:ls) = if fst l then rank ls ++ [[snd l]] else [[snd l]] ++ rank ls

--instance Rankable Bool where

instance (Rankable key1, Rankable key2) => Rankable (key1,key2) where
  rank = permutations . map snd . digitalSortOn fst . map snd . digitalSortOn fst . map assoc
--permutations is wrong but I put in in here so the function runs, should be a groupBy somewhere to make the
--function work properly

assoc :: ((k1,k2),a) -> (k1,(k2,a))
assoc ((k1,k2),a) = (k1,(k2,a))

--etc.
