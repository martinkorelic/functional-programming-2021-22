module Exercise6 where

{-

(++) = flip (foldr (:))
concat = foldr (++) []
reverse = foldr (\x r -> r ++ [x]) []

foldr op e [] = e
foldr op e (x:xs) = x ‘op‘ (foldrop e xs)

f . foldr g e = foldr h (f e) <= f (g x y) = h x (f y)

Show:
    reverse . concat = foldr (flip (++) . reverse) []

    -----
    Rewrite `concat` as folds:
        concat = foldr (++) []

    Filling in the FF-law:
        reverse . foldr (++) [] = foldr (flip (++) . reverse) []

    Hence:
        f = reverse
        g = (++)
        h = flip (++) . reverse
    -----

    Show that:
        1. [] = f []
    	2. f (g x y) = h x (f y)

        =>

        1. [] = reverse [] 
        2. reverse (x ++ y) = flip (++) . reverse x (reverse y)

        =>
        1. [] = reverse [] { def. of reverse }
        2. reverse (x ++ y)
            = { property of reverse }
            reverse y ++ reverse x
            = { def. flip }
            flip (++) (reverse x) (reverse y)
            = { def. (.) }
            flip (++) . reverse x (reverse y)

-}          


{-

Show:
    foldr k e (xs ++ ys) = foldr k (foldr k e ys) xs

    -----
    Rewrite:
        foldr k e (xs ++ ys) = foldr k e (foldr (:) ys xs)

    Filling in the FF-law:
        foldr k e (foldr (:) ys xs) = foldr k (foldr k e ys) xs

    Hence:
        f = foldr k e
        g = (:)
        h = k
    -----

    Show that:
        1. foldr k e ys = foldr k e ys { obvious }
        2. foldr k e (a : b) = k a (foldr k e b) { def. foldr }

-}

{-

foldr f e (reverse xs) = foldl (flip f e xs)

Case 1: xs = []
    foldr f e (reverse []) = foldl (flip f) e []

    foldr f e (reverse [])
    = { def. reverse }
    foldr f e []
    = { def. foldr }
    e
    = { def. foldl }
    foldl f e []
    = { def. flip }
    foldl (flip f) e []

Case 2: xs = a:as

    IH: foldr f e (reverse as) = foldl (flip f) e as

    foldr f e (reverse (a:as))
    = { def. reverse }
    foldr f e (foldr (\x r -> r ++ [x]) [] (a:as))
    = { def. foldr, reverse backwards }
    foldr f e (reverse as ++ [a])
    = { prop. foldr }
    foldr f (foldr f e [a]) (reverse as)
    = { def. foldr (2 times) }
    foldr f (f a e) (reverse as)
    = { IH. }
    foldl (flip f) (f a e) as
    = { def. flip }
    foldl (flip f) (flip f e a) as
    = { def. foldl }
    foldl (flip f) e (a:as)

-}