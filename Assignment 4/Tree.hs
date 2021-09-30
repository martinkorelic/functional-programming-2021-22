module Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

tr :: Tree Integer
tr = Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) (Node 4 (Node 5 Leaf Leaf) Leaf)

{- 
          3
        2   4
      1  7 5  x
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

--inOrder :: Tree a -> [a]
--fromAscList :: [a] -> Tree a
--breadthFirst :: Tree a -> [a]

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
