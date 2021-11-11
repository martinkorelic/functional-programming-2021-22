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


LHS:
map f (tips (Tip a))
= { definition of tips }
map f [a]
= { def. of map }
[f a]


RHS:
tips (mapBtree f (Tip a))
= { def. of mapBtree }
tips (Tip (f a))
= { def. of tips }
[f a]

LHS = RHS


Induction hypothesis:

Assume:
P(t1) => for all x, P(Bin t1 x) : map f (tips (Bin t1 x)) = tips (mapBtree f (Bin t1 x)) - IH.1
P(t2) => for all x, P(Bin x t2) : map f (tips (Bin x t2)) = tips (mapBtree f (Bin x t2)) - IH.2

Show that: map f (tips (Bin t1 t2)) = tips (mapBtree f (Bin t1 t2))

LHS:
map f (tips (Bin t1 t2))
= { def. of tips }
map f (tips t1 ++ tips t2)
= { def. of map }
[f tips t1, f tips t2]


RHS:
tips (mapBtree f (Bin t1 t2))
= { def. of mapBtree }
tips (Bin (mapBtree f t1) (mapBtree f t2))
= { def. of tips }
tips (mapBtree f t1) ++ tips (mapBtree f t2)
= { IH.1 }
map f (tips t1) ++ tips (mapBtree f t2)
= { IH.2 }
map f (tips t1) ++ map f (tips t2)
= { def. of tips }
[f tips t1] ++ [f tips t2]
= { def. of ++ }
[f tips t1, f tips t2]

LHS = RHS