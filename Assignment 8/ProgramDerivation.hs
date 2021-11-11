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

Base case:

P(Leaf)

inorderCat Leaf xs
= { def. of inorderCat }
inorder Leaf ++ xs
= { def. of inorder }
[] ++ xs
= { def. of ++ }
xs

Induction hypothesis:

Assume:
P(t1), for all x, P(Node a t1 x) => inorderCat (Node a t1 x) xs = inorder (Node a t1 x) ++ xs - IH.1
P(t2), for all x, P(Node a x t2) => inorderCat (Node a x t2) xs = inorder (Node a x t2) ++ xs - IH.2

inorderCat (Node a t1 t2) xs
= { def. of inorderCat }
inorder (Node a t1 t2) ++ xs
= { def. of inorder }
inorder t1 ++ [a] ++ inorder t2 ++ xs
= { IH.1 }
inorderCat t1 [a] ++ inorder t2 ++ xs
= { IH. 2 }
inorderCat t1 [a] ++ inorderCat t2 xs
= { com. of ++ }
inorderCat t2 xs ++ inorderCat t1 [a]
-}

inorderCat :: Tree a -> [a] -> [a]
--inorderCat t xs = inorder t ++ xs -- TODO: make me more efficient
inorderCat Leaf xs = xs
inorderCat (Node a t1 t2) xs = inorderCat t2 xs ++ inorderCat t1 [a]

-- New implementation is more efficient
inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- TODO: make me more efficient, too
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

{-

elemsCat t xs = elems t ++ xs

Base case:

P(Leaf)

elemsCat Leaf xs
= { def. of elemsCat }
elems Leaf ++ xs
= { def. of elems }
[] ++ xs
= { def. of ++ }
xs

Induction hypothesis:

Assume:
P(t1), for all x, P(Node a t1 x) => elemsCat (Node a t1 x) xs = elems (Node a t1 x) ++ xs
P(t2), for all x, P(Node a x t2) => elemsCat (Node a x t2) xs = elems (Node a x t2) ++ xs

elemsCat (Node a t1 t2) xs
= { def. of elemsCat }
elems (Node a t1 t2) ++ xs
= { def. of elems }
x : elems t1 ++ elems t2 ++ xs
= { def. of. ++ }
([x] ++ elems t1) ++ elems t2 ++ xs
= { com. of ++ }
elems t1 ++ [x] ++ elems t2 ++ xs
= { IH.1 }
elemsCat t1 [x] ++ elems t2 ++ xs
= { IH.2 }
elemsCat t1 [x] ++ elemsCat t2 xs

-}

elemsCat :: Tree a -> [a] -> [a]
elemsCat Leaf xs = xs
elemsCat (Node x t1 t2) xs = elemsCat t2 xs ++ elemsCat t1 [x]

elems' :: Tree a -> [a]
elems' t = elemsCat t []