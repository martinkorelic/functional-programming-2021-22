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
P(t1) => for all x, P(t1) : map f (tips t1) = tips (mapBtree f t1) - IH.1
P(t2) => for all x, P(t2) : map f (tips t2) = tips (mapBtree f t2) - IH.2

Show that: map f (tips (Bin t1 t2)) = tips (mapBtree f (Bin t1 t2))

map f (tips (Bin t1 t2))
= { def. of tips }
map f (tips t1 ++ tips t2)
= { def. map }
map f (tips t1) ++ map f (tips t2)
= { IH.2 }
map f (tips t1) ++ tips (mapBtree f t2)
= { IH.1 }
tips (mapBtree f t1) ++ tips (mapBtree f t2)
= { def. tips }
tips (Bin (mapBtree f t1) (mapBtree f t2))
= { def. mapBtree }
tips (mapBtree f (Bin t1 t2))