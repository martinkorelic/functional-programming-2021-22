> module Btree where
>
> data Btree a = Tip a | Bin (Btree a) (Btree a)
>
> mapBtree :: (a -> b) -> Btree a -> Btree b
> mapBtree f (Tip a)     = Tip (f a)
> mapBtree f (Bin t1 t2) = Bin (mapBtree f t1) (mapBtree f t2)

> tips :: (Btree a) -> [a]
> tips (Tip x) = [x]
> tips (Bin as bs) = tips as ++ tips bs

Base case:

P(Tip a)

Left side:

map f (tips (Tip a))
= { definition of tips }
map f [a]
= { def. of map }
[f a]

Right side:

tips (mapBtree f (Tip a))
= { def. of mapBtree }
tips (Tip (f a))
= { def. of tips }
[f a]

Induction hypothesis:

P(sTree) => for all x,
P(Bin sTree (Btree x))
P(Bin (Btree x) sTree)

map f (tips (Bin sTree (Btree x)))
= { def. of tips }
map f (tips sTree ++ tips x)
= { def. of tips }
map f ([sTree] ++ [x])
= { def. of ++ }
map f [sTree, x]
= { def. of map }
[f sTree, f x]

tips (mapBtree f (Bin sTree (Btree x))
= { def. of mapBtree }
tips (Bin (mapBtree f sTree) (mapBtree f x))
= { def. of mapBtree }
tips (Bin (Tip (f sTree) (Tip (f x))))
= { def. of tips }
tips (f sTree) ++ tips (f x)
= { def. of tips }
[f sTree] ++ [f x]
= { def. of ++ }
[f sTree, f x]

To prove: map f (tips t) = tips (mapBtree f t) for all f,t