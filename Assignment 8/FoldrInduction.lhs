Local definitions:

> import Prelude hiding (foldr)

> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr f b [] = b
> foldr f b (x:xs) = f x (foldr f b xs)
>
> compose :: [a -> a] -> a -> a
> compose [] = id
> compose (f:fs) = f . compose fs

-----------------------------------------------------
To prove: foldr f b xs = compose (map f xs) b
By induction on xs.

Case 1: xs = []

    foldr f b []
    =
    b
    =
    id b
    =
    compose [] b
    =
    compose (map f []) b

Case 2: xs = (a:as)
IH: foldr f b as = compose (map f as) b

    foldr f b (a:as)
    =
    f a (foldr f b as)
    =
    f a (compose (map f as) b)
    =
    (f a . compose (map f as)) b
    =
    compose (f a : map f as) b
    =
    compose (map f (a:as)) b