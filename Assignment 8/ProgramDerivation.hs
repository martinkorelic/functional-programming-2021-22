module Expression where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

skewed :: Integer -> Tree ()
skewed 0 = Leaf
skewed n = Node () (skewed (n-1)) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

{-
Derive
  inorderCat t xs = inorder t ++ xs

P(xs = [], t = Leaf)

inorderCat Leaf xs
=
inorder Leaf ++ xs
=
[] ++ xs
=
xs

P(sTree) => for all x,
P(Node x sTree (Tree x))

inorderCat (Node x sTree (Tree x)) xs
=
inorder (Node x sTree (Tree x)) ++ xs
=
inorder sTree ++ [x] ++ inorder x ++ xs
=
inorderCat sTree [x] ++ inorderCat x xs


inorderCat Leaf xs = xs
inorderCat (Node x sTree (Tree x)) xs = inorderCat sTree [x] ++ inorderCat x xs

inorder' t = inorderCat t []

-}

inorderCat :: Tree a -> [a] -> [a]
--inorderCat t xs = inorder t ++ xs -- TODO: make me more efficient
inorderCat Leaf xs = xs
inorderCat (Node x x1 x2) xs = inorderCat x1 [x] ++ inorderCat x2 xs

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- TODO: make me more efficient, too
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt