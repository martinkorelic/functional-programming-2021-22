module Exercise3 where

f5 f x y z = f (f x y) z

{-

f5 :: 1 -> 2 -> 3 -> 4 -> 5
    f :: 1 = ?
    x :: 2 = ?
    y :: 3 = ?
    z :: 4 = ?
    rhs :: 5 = ?
    -----
        f :: 6 -> 4 -> 8
        8 = 5
        8 = 9

    -----
        f :: 2 -> 3 -> 9
        9 = 6

f5 :: 1 -> 2 -> 3 -> 4 -> 5
    f :: 1 = ?
    x :: 2 = ?
    y :: 3 = ?
    z :: 4 = ?
    rhs :: 5 = ?
    -----
        f :: a -> 4 -> 8
        8 = 5
        8 = a

    -----
        f :: 2 -> 3 -> a
        a = a

f5 :: 1 -> 2 -> 3 -> 4 -> 5
    f :: 1 = (a -> 4 -> a)
    x :: 2 = a
    y :: 3 = ?
    z :: 4 = ?
    rhs :: 5 = a
    -----
        f :: a -> 4 -> a
        a = a
        a = a

    -----
        f :: a -> 3 -> a
        a = a

f5 :: 1 -> 2 -> 3 -> 4 -> 5
    f :: 1 = (a -> b -> a)
    x :: 2 = a
    y :: 3 = b
    z :: 4 = b
    rhs :: 5 = a
    -----
        f :: a -> b -> a
        a = a
        a = a

    -----
        f :: a -> b -> a
        a = a

f5 :: (a -> b -> a) -> a -> b -> b -> a

-}

f6 xs = reverse [(y,x) | (x,y) <- xs]

{-

f6 :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    -----
    (<-) RH
        xs = [3]
        [3] = 1
    -----
    LH (<-)
        (x,y) = (4,5)
        3 = (4,5)
    -----
    reverse :: [a] -> [a]


f6 :: 1 -> 2
    xs :: 1 = [(b,a)]
    rhs :: 2 = [(a,b)]
    -----
    (<-) RH
        xs = [3]
        [(b,a)] = 1
    -----
    LH (<-)
        (x,y) = (b,a)
        3 = (b,a)
    -----
    (|)
        (b,a) = (a,b)
    -----
    reverse :: [a] -> [a]

f6 :: [(b,a)] -> [(a,b)]

-}

f7 :: (Foldable t, Num a) => t a -> [a]
f7 = foldMap (\x -> [x + 10, x + 20])

{-

foldMap :: Monoid m => (a -> m) -> f a -> m

(a -> m) = (\x -> [x + 10, x + 20])

    x = a
    m = [x + 10, x + 20] :: [a]


f7 :: [a] -> [a]

f7 :: (Foldable t, Num a) => t a -> [a]

-}

f8 f = do
    x <- f 0
    case x of
        Nothing -> f 1
        y -> return y

{-

f8 :: (Monad m, Num a) => (a -> m (Maybe a)) -> m (Maybe a)

-}