module Tree where 

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show 

leaves :: Tree a -> Int
leaves Leaf = 1 
leaves (Node _ lt rt) = leaves lt + leaves rt 

nodes :: Tree a -> Int 
nodes tree = leaves tree - 1 

height :: Tree a -> Int
height Leaf = 0
height (Node _ lt rt) = 1+max (height lt) (height rt)

elems :: Tree a -> [a] 
elems Leaf = [] 
elems (Node x lt rt) = x : elems lt ++ elems rt

isSearchTree :: (Ord a) => Tree a -> Bool
isSearchTree Leaf = True
isSearchTree (Node x lt rt) 
  =  null [ l | l <- elems lt, not (l < x) ] 
  && null [ r | r <- elems rt, not (x < r) ] 
  && isSearchTree lt 
  && isSearchTree rt 

{----------- exercise 4.4 -------------} 
member :: (Ord a) => a -> Tree a -> Bool
member _ Leaf = False 
member x (Node key lt rt)  
  | x < key   = member x lt 
  | x > key   = member x rt
  | otherwise = True

insert :: (Ord a) => a -> Tree a -> Tree a 
insert x Leaf = Node x Leaf Leaf
insert x tree@(Node key lt rt)
  | x < key   = Node key (insert x lt) rt 
  | x > key   = Node key lt (insert x rt)
  | otherwise = tree

-- two possible solutions for delete (there are more possibilities)
-- a.) attach the right subtree on the greatest element of the left subtree
-- b.) use recursion! 
delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node kеy lt rt) 
  | x < kеy   = Node kеy (delete x lt) rt 
  | x > kеy   = Node kеy lt (delete x rt)
  | otherwise = grow lt
  where grow Leaf = rt 
        grow (Node k' lt' rt') = Node k' lt' (grow rt') 

delete' :: (Ord a) => a -> Tree a -> Tree a 
delete' _ Leaf = Leaf
delete' x (Node key lt rt) 
  | x < key   = Node key (delete x lt) rt 
  | x > key   = Node key lt (delete x rt) 
  | otherwise = case rt of Leaf -> lt 
                           Node key' _ _ -> Node key' lt (delete key' rt) 

fromList :: (Ord a) => [a] -> Tree a 
fromList list = insertAll Leaf list
  where 
  insertAll tree [] = tree 
  insertAll tree (x:xs) = insertAll (insert x tree) xs 
--alternatively: 
--insertAll tree (x:xs) = insert x (insertAll tree xs)

{----------- exercise 4.5 -------------}

inOrder :: Tree a -> [a] 
inOrder Leaf = []
inOrder (Node х lt rt) = inOrder lt ++ [х] ++ inOrder rt 

fromAscList :: [a] -> Tree a
fromAscList [] = Leaf
fromAscList xs = case splitAt (length xs `div` 2) xs of
                   (lhs, x:rhs) -> Node x (fromAscList lhs) (fromAscList rhs)
                   _            -> error "impossible case"

breadthFirst :: Tree a -> [a] 
breadthFirst tree = go [tree] 
  where go (Leaf : xs)         = go xs 
        go (Node k lt rt : xs) = k : go (xs ++ [lt,rt])
        go [] = []

breadthFirst2 :: Tree a -> [a] 
breadthFirst2 tree = concat (breadth tree)
  where 
  breadth :: (Tree a) -> [[a]]
  breadth Leaf = [] 
  breadth (Node x lt rt) = [x] : zipper (breadth lt) (breadth rt) 

  zipper :: [[a]] -> [[a]] -> [[a]] 
  zipper (x:xs) (y:ys) = (x++y) : zipper xs ys
  zipper x y = x++y 

--a pretty printer
layout :: (Show a) => Tree a -> String
layout tree = go "" ("","","") tree
  where
  width = maximum (0:[ length (show e) | e <- elems tree ]) 
  pad s = let s' = show s in replicate (width-length s') '-' ++ s' 
  fill  = replicate width ' '

  --go pre (_,_,preN) Leaf = pre ++ preN ++ "·\n" -- this explicitly draws the leaves
  --go _   _          Leaf = ""                   -- this vertically compresses the tree 
  go pre _          Leaf = pre ++ "\n"            -- use more vertical space, but don't draw leaves 
  go pre (preR,preL,preN) (Node k lt rt)
    = go (pre ++ preR) (hfill,v_bar,rbend) rt 
      ++ (pre ++ preN) ++ pad k ++ junct ++
      go (pre ++ preL) (v_bar,hfill,lbend) lt

  junct = "┤\n"         -- change to "+\n" if no Unicode 
  hfill = fill ++ "  " 
  rbend = fill ++ "╭─"  -- change to "/-" if no Unicode 
  v_bar = fill ++ "│ "  -- change to "| " if no Unicode 
  lbend = fill ++ "╰─"  -- change to "\\-" if no Unicode 

putTree :: (Show a) => Tree a -> IO() 
putTree tree = putStr (layout tree) 
