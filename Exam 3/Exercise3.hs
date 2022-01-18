{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exercise3 where

data QTree = Black | White | Node (QTree, QTree, QTree, QTree)
    deriving (Show)

listToT4 [a,b,c,d] = (a,b,c,d)
t4ToList (a,b,c,d) = [a,b,c,d]

size :: QTree -> Int
size Black = 1
size White = 1
size (Node sts) = 1 + sum (map size (t4ToList sts))

diff :: QTree -> Int
diff Black = -1
diff White = 1
diff (Node sts) = sum (map size (t4ToList sts))

data Bit = O | I
    deriving (Eq, Ord, Show)

compress :: QTree -> [Bit]
compress Black = O:[O]
compress White = O:[I]
compress (Node sts) = I:concat (map (\x -> compress x) (t4ToList sts))

decompress :: [Bit] -> QTree
decompress = head . decompr where
    decompr [ ] = [ ]
    decompr (O : O : bs) = Black : decompr bs
    decompr (O : I : bs) = White : decompr bs
    decompr (I : bs) = let lqts = decompr bs in Node (listToT4 (take 4 lqts)) : drop 4 lqts