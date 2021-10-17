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

--bv :: [((Int,Int), Int)]
--bv = zip [(1,3), (4,3), (1,3), (8,9), (2,2), (0,0)] [1,2,3,4,5,6]
--mr :: [(String, Int)]
--mr = zip ["ah", "aah", "ch", "deh", "fh", "deh"] [1,2,3,4,5,6]

instance Rankable Bool where
  rank [] = [[],[]]
  rank (l:ls)
    | fst l = let h = rank ls in [head h, snd l : last h] 
    | otherwise = let h = rank ls in [snd l : head h, last h]

instance (Rankable key1, Rankable key2) => Rankable (key1,key2) where
  rank = concat . map rank . rank . map assoc

assoc :: ((k1,k2),a) -> (k1,(k2,a))
assoc ((k1,k2),a) = (k1,(k2,a))

maybRank :: [(Maybe key, a)] -> [[(Maybe key, a)]]
maybRank [] = [[],[]]
maybRank (l:ls)
  | isNothing (fst l) = let h = maybRank ls in [l : head h, last h]
  | otherwise = let h = maybRank ls in [head h, l : last h]

instance Rankable key => Rankable (Maybe key) where
  rank l = [ x | x <- cList, not (null x) ]
    where mList = maybRank l
          cList = (map snd (head mList)) : (rank $ map (\(Just x,y) -> (x,y)) (last mList))

instance Rankable key => Rankable [key] where
  rank = rank . digitalSortOn fst . map (\(x, y) -> (uncons x, y))

rankWithKey :: (Rankable key) => [(key,a)] -> [[(key,a)]]
rankWithKey l = map (\(x,y) -> zip x y) $ zip ordKeys ordValues
            where ordKeys = rank (map (\(k,v) -> (k,k)) l)
                  ordValues = rank l