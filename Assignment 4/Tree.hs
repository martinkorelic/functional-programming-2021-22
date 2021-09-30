module Tree where

import Data.List as List (reverse)

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

tr :: Tree Integer
tr = Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) (Node 6 (Node 4 Leaf Leaf) (Node 8 Leaf Leaf))

{- 
          3
        2   6
      1  x 4  5
     x  x x x
 -}


{----------- exercise 4.3 -------------}

leaves :: Tree a -> Int
leaves Leaf = 1
leaves (Node a l r) = leaves l + leaves r

nodes  :: Tree a -> Int
nodes Leaf = 0
nodes (Node a l r) = 1 + nodes l + nodes r

-- If a binary tree has n leaves, then it has n-1 nodes.

height :: Tree a -> Int
height Leaf = 0
height (Node a l r) = 1 + max (height l) (height r)

elems  :: Tree a -> [a]
elems Leaf = []
elems (Node a l r) = elems l ++ [a] ++ elems r

isSearchTree :: (Ord a) => Tree a -> Bool
isSearchTree Leaf = True
isSearchTree (Node a l r)
  | length (elems l) <= 3 = isSorted (elems l) && isSearchTree r
  | length (elems r) <= 3 = isSorted (elems r) && isSearchTree l
  | otherwise = isSearchTree l && isSearchTree r

-- Helper function
isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all (\(x, y) -> x <= y) $ zip xs (tail xs)

{----------- exercise 4.4 -------------}

member :: (Ord a) => a -> Tree a -> Bool
member a Leaf = False
member a (Node b l r) = a == b || member a l || member a r

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node v l r)
  | x < v = Node v (insert x l) r
  | x > v = Node v l (insert x r)
  | otherwise = Node v l r

delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node v l r)
  | x < v = Node v (delete x l) r
  | x > v = Node v l (delete x r)
delete _ (Node _ Leaf Leaf) = Leaf
delete _ (Node _ l Leaf) = l
delete _ (Node _ Leaf r) = r
delete _ (Node _ l r) = Node value l (delete value r)
  where value = minimum (elems r)

fromList :: (Ord a) => [a] -> Tree a
fromList x = listTree Leaf x

listTree :: (Ord a) => Tree a -> [a] -> Tree a
listTree t [] = t
listTree t (ha:ta) = listTree (insert ha t) ta

{----------- exercise 4.5 -------------}

inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node v Leaf Leaf) = [v]
inOrder (Node v l Leaf) = inOrder l ++ [v]
inOrder (Node v Leaf r) = [v] ++ inOrder r
inOrder (Node v l r) = inOrder l ++ [v] ++ inOrder r

fromAscList :: [a] -> Tree a
fromAscList [] = Leaf
fromAscList xs =  Node (xs !! h) (fromAscList (reverse (divide (reverse xs) sh))) (fromAscList (divide xs fh)) 
      where fh = length xs `div` 2
            h = fh
            sh = length xs - h - 1

divide :: (Num a1, Enum a1, Ord a1) => [a2] -> a1 -> [a2]
divide xs h = [ x | (i,x) <- zip [0..] xs, i > h ]

breadthFirst :: Tree a -> [a]
breadthFirst t = bFifo [t] t

bFifo :: [Tree a] -> Tree a -> [a]
bFifo q Leaf = []
bFifo [] _ = []
bFifo (q:qs) (Node v l Leaf) = [v] ++ bFifo qr (head qr)
  where qr = qs ++ [l]

bFifo (q:qs) (Node v Leaf r) = [v] ++ bFifo qr (head qr)
  where qr = qs ++ [r]

bFifo q (Node v l r) = [v] ++ bFifo qr (head qr)
  where qr = tail q ++ [l] ++ [r]

{- BONUS: a tree pretty printer; the recursive structure of this function
 - is prety simple, but it is a fiddly function to write if you want it to
 - produce an actually nice tree. -}

layout :: (Show a) => Tree a -> String
layout tree = go "" ("","","") tree ++ "\n"
  where
  width = maximum (0:[ length (show e) | e <- elems tree ])
  pad s = let s' = show s in replicate (width-length s') '-' ++ s'
  fill  = replicate width ' '

  go pre _ Leaf = pre ++ "\n" -- change this to "" to get a more compact display
  go pre (preL,preR,preN) (Node k lt rt)
    = go (pre ++ preL) (hfill,v_bar,lbend) rt
      ++ (pre ++ preN) ++ pad k ++ junct ++
      go (pre ++ preR) (v_bar,hfill,rbend) lt

  junct = "┤\n"
  hfill = fill ++ "  "
  lbend = fill ++ "╭─"  -- change to "/-" if no Unicode
  v_bar = fill ++ "│ "  -- change to "| " if no Unicode
  rbend = fill ++ "╰─"  -- change to "\-" if on Unicode

putTree :: (Show a) => Tree a -> IO()
putTree tree = putStr (layout tree)
