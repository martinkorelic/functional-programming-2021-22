> module TreeInduction where
> 
> data Tree a = Leaf | Node a (Tree a) (Tree a)
>   deriving (Show)

1: What is the induction scheme for trees?

    P(Leaf)
    P(bin1) => forall x, any a, P(Node a bin1 x)
    P(bin2) => forall y, any a, P(Node a y bin2)

Local definitions:

> leaves :: Tree a -> Int
> leaves Leaf = 1
> leaves (Node _ l r) = leaves l + leaves r
>
> nodes :: Tree a -> Int
> nodes Leaf = 0
> nodes (Node _ l r) = 1 + nodes l + nodes r

2: To prove: leaves t = nodes t + 1
By induction on t.

Case 1: P(Leaf)

    leaves Leaf 
    = { def. leaves }
    1
    = { add 0 }
    0 + 1
    = { def. nodes }
    nodes Leaf + 1 

Case 2: t = Node a bin1 bin2

P(bin1) => for all x, any a, P(bin1) : leaves bin1 = nodes bin1 + 1 - IH.1
P(bin2) => for all x, any a, P(bin2) : leaves bin2 = nodes bin2 + 1 - IH.2

leaves (Node a bin1 bin2) = nodes (Node a bin1 bin2) + 1

    leaves (Node a bin1 bin2)
    = { def. leaves }
    leaves bin1 + leaves bin2
    = { IH.1 }
    nodes bin1 + 1 + leaves bin2
    = { IH.2 }
    nodes bin1 + 1 + nodes bin2 + 1
    = { commutativity }
    1 + nodes bin1 + nodes bin2 + 1
    = { def. nodes }
    nodes (Node a bin1 bin2) + 1