module PolyTypes where

mingle :: [a] -> [a] -> [a]
mingle xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

{-
mingle :: 1 -> 2 -> 3
    xs :: 1 = ?
    ys :: 2 = ?
    rhs :: 3 = ?
    3 = [4]
    (<-r):
        zip :: [5] -> [6] -> [(5, 6)]
        xs = [5] = 1
        ys = [6] = 2
    (l<-):
        (7,8) = (5,6)
        x :: 7
        y :: 8

mingle :: 1 -> 2 -> 3
    xs :: 1 = ?
    ys :: 2 = ?
    rhs :: 3 = ?
    3 = [4]
    (<-r):
        zip :: [5] -> [6] -> [(5, 6)]
        xs = [5] = 1
        ys = [6] = 2
    (l<-):
        (7,8) = (5,6)
        x :: 7
        y :: 8
    (<-r*):
        [9, 10]
        9 = 7
        10 = 8
    (l<-*):
        e :: 11
        11 = [9, 10]
        9 = 10

mingle :: 1 -> 2 -> 3
    xs :: [a] = ?
    ys :: [b] = ?
    rhs :: [[a,b]] = ?
    [[a,b]] = [a,b]
    (<-r):
        zip :: [a] -> [b] -> [(a, a)]
        xs = [a] = a
        ys = [a] = a
    (l<-):
        (a,a) = (a,a)
        x :: a
        y :: a
    (<-r*):
        [a, a]
        a = a
        a = a
    (l<-*):
        e :: a
        a = [a]

mingle :: [a] -> [a] -> [a]

-}

sumWith g xs = foldr (+) 0 (map g xs)

{-

sumWith :: 1 -> 2 -> 3
    g :: 1 = ?
    xs :: 2 = ?
    rhs :: 3 = ?

    map :: (4 -> 5) -> [4] -> [5]
        1 = (4 -> 5)
        2 = [4]

    foldr :: (6 -> 7 -> 7) -> 7 -> Foldable 6 -> 7
        (+) = (6 -> 7 -> 7)
        7 = 0
        Foldable 6 = [5]

sumWith :: 1 -> 2 -> 3
    g :: 1 = ?
    xs :: 2 = ?
    rhs :: 3 = ?

    map :: (4 -> 5) -> [4] -> [5]
        1 = (4 -> 5)
        2 = [4]
        [5] = a

    foldr :: (a -> b -> b) -> b -> [a] -> b
        (+) = (a -> b -> b)
        b = 0
        [a] = [a]

sumWith :: 1 -> 2 -> 3
    g :: 1 = ?
    xs :: 2 = ?
    rhs :: 3 = ?

    map :: (b -> a) -> [b] -> [a]
        1 = (b -> a)
        2 = [b]
        [a] = a

    foldr :: (a -> b -> b) -> b -> [a] -> b
        (+) = (a -> b -> b)
        b = 0
        [a] = [a]

sumWith :: 1 -> 2 -> 3
    g :: 1 = (b -> a)
    xs :: 2 = [b]
    rhs :: 3 = b

    map :: (b -> a) -> [b] -> [a]
        1 = (b -> a)
        2 = [b]
        [a] = a

    foldr :: (a -> b -> b) -> b -> [a] -> b
        (+) = (a -> b -> b)
        b = 0
        [a] = [a]

sumWith :: (b -> a) -> [b] -> a

-}

transform :: (a1 -> [a2]) -> [a1] -> [a2]
transform f  = concat . map f

{-
transform f  = concat . map f
transform f g = concat $ map f g

transform :: 1 -> 2 -> 3
    f :: 1 = ?
    g :: 2 = ?
    rhs :: 3 = ?

    map :: (4 -> 5) -> [4] -> [5]
        1 = (4 -> 5)
        2 = [4]

    concat :: [[6]] -> [6]
        [[6]] = [5]
        [6] = 3

transform :: 1 -> 2 -> 3
    f :: 1 = ?
    g :: 2 = ?
    rhs :: [a] = ?

    map :: (b -> [a]) -> [b] -> [a]
        1 = (b -> [a])
        2 = [b]

    concat :: [[a]] -> [a]
        [[a]] = [a]
        [a] = a


transform :: (b -> [a]) -> [b] -> [a]

-}