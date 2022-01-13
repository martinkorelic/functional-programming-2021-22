module PolyTypes where

justs :: [Maybe Char] -> [Char]
justs xs           = [ x | Just x <- xs, x /= ' ' ]

{-

justs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?

justs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    l<- :: 3
    <-r :: 4

justs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    l<- :: 3 4
    <-r :: [3 4]
    1 = [3 4]

justs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    l(<-) :: 3 4
    (<-)r :: [3 4]
    1 = [3 4]

justs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    l(<-) :: 3 4
    (<-)r :: [3 4]
    1 = [3 4]
    x :: 4
    l(/=) :: 4
    (/=)r :: 4
    l | :: 4
    2 = [4]

justs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    l(<-) :: 3 Char
    (<-)r :: [3 Char]
    1 = [3 Char]
    x :: Char
    l(/=) :: Char
    (/=)r :: Char
    l | :: Char
    2 = [Char]

justs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    l(<-) :: Maybe Char
    (<-)r :: [Maybe Char]
    1 = [Maybe Char]
    x :: Char
    l(/=) :: Char
    (/=)r :: Char
    l | :: Char
    2 = [Char]

justs :: 1 -> 2
    xs :: [Maybe Char]
    rhs :: 2 = [Char]
    l(<-) :: Maybe Char
    (<-)r :: [Maybe Char]
    1 = [Maybe Char]
    x :: Char
    l(/=) :: Char
    (/=)r :: Char
    l | :: Char
    2 = [Char]

justs :: [Maybe Char] -> [Char]

-}


orderPairs xs      = map (\(x,y)->(min x y, max x y)) xs

{-

orderPairs :: 1 -> 2

orderPairs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    map :: (3 -> 4) -> [3] -> [4]

orderPairs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    map :: (3 -> 4) -> [3] -> [4]
    (x,y) = (5,6)
    x :: 5 = ?
    y :: 6 = ?

orderPairs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    map :: (3 -> 4) -> [3] -> [4]
    left:
    (x,y) = (5,6)
    x :: 5 = ?
    y :: 6 = ?
    right: (9,10)
    min :: (7 -> 7 -> 7)
    min :: (8 -> 8 -> 8)
    5 = 7
    6 = 7
    5 = 8
    6 = 8
    7 = 9
    8 = 10
    (5,6) = (9,10)

orderPairs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    map :: (3 -> 4) -> [3] -> [4]
    left:
    (x,y) = (5,6)
    x :: 5 = ?
    y :: 6 = ?
    right: (9,10)
    min :: Ord 7 => (7 -> 7 -> 7)
    min :: Ord 8 => (8 -> 8 -> 8)
    5 = 7
    6 = 7
    5 = 8
    6 = 8
    7 = 9
    8 = 10

orderPairs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    map :: (3 -> 4) -> [3] -> [4]
    3 = left:(5,6)
    4 = right:(5,6)
    1 = [3]
    2 = [4]

orderPairs :: 1 -> 2
    xs :: 1 = ?
    rhs :: 2 = ?
    map :: ((5,6) -> (5,6)) -> [(5,6)] -> [(5,6)]
    3 = left:(5,6)
    4 = right:(5,6)
    2 = [(5,6)]

orderPairs :: 1 -> 2
    xs :: 1 = [(5,6)]
    rhs :: 2 = [(5,6)]
    map :: ((5,6) -> (5,6)) -> [(5,6)] -> [(5,6)]
    3 = left:(5,6)
    4 = right:(5,6)
    1 = [(5,6)]
    2 = [(5,6)]
    Ord (5,6) => 5,6
    Ord (a,b) => (a,b) 

orderPairs :: Ord (a,b) => [(a,b)] -> [(a,b)]
-}

unmaybe (Just x)   = x
unmaybe Nothing    = Nothing

{-

unmaybe :: 1 -> 2
    (Just x) :: 3 (5 x) = ?
    Nothing :: 4 = ?
    x :: 2 = ?
    Nothing :: 2 = ?
    
unmaybe :: 1 -> 2
    (Just x) :: Maybe (Maybe 3)
    Nothing :: Maybe 3
    x :: 3 = ?
    Nothing :: Maybe 3
    Maybe 3 => Just 3, Nothing

unmaybe :: 1 -> 2
    1=(Just x) :: Maybe (Maybe a)
    2=Nothing :: Maybe a
    x :: a = ?
    Nothing :: Maybe a
    Maybe a => Just a, Nothing

unmaybe :: Maybe (Maybe a) -> Maybe a
-}

accumulate f st    = let (x,st') = f st in x : accumulate f st'

{-

accumulate :: 1 -> 2 -> 3
    f :: 1 = ?
    st :: 2 = ?
    rhs :: 3 = ?

accumulate :: 1 -> 2 -> 3
    f :: 1 = ?
    st :: 2 = ?
    rhs :: 3 = ?
    let (*) in (**)
    (*): (f @ st) :: (4 -> 5) -> 4 -> 5
    (**): (accumulate @ f @ st') :: (1 -> 2 -> 3) -> 1 -> 2 -> 3

accumulate :: 1 -> 2 -> 3
    f :: 1 = ?
    st :: 2 = ?
    rhs :: 3 = ?
    let (*) in (**)
    (*): 
        (f @ st) :: (4 -> 5) -> 4 -> 5
        (x,st') :: (6,7)
        (6,7) = 5
    (**):
        (accumulate @ f @ st') :: (1 -> 2 -> 3) -> 1 -> 2 -> 3
        x :: 8
        (:) :: 9 -> [9] -> [9]
        8 = 9
        [9] = 3
        f = 1
        st' :: 2 = 7
        x :: 8 = 6

accumulate :: 1 -> 2 -> 3
    f :: 1 = ?
    st :: 2 = ?
    rhs :: 3 = ?
    let (*) in (**)
    (*): 
        (f @ st) :: (4 -> 5) -> 4 -> 5
        f :: (4 -> 5)
        st :: 4
        (x,st') :: (6,7)
        (6,7) = 5
    (**):
        (accumulate @ f @ st') :: (1 -> 2 -> 3) -> 1 -> 2 -> 3
        x :: 8
        (:) :: 9 -> [9] -> [9]
        8 = 9
        [9] = 3
        f = 1
        st' :: 2 = 7
        x :: 8 = 6
        rhs1 :: [9]

accumulate :: 1 -> 2 -> 3
    f :: 1 = ?
    st :: 2 = ?
    rhs :: 3 = ?
    let (*) in (**)
    (*): 
        (f @ st) :: (4 -> 5) -> 4 -> 5
        f :: (4 -> 5)
        st :: 4
        (x,st') :: (a,7)
        (a,7) = 5
    (**):
        (accumulate @ f @ st') :: (1 -> 2 -> [a]) -> 1 -> 2 -> [a]
        x :: 8
        (:) :: a -> [a] -> [a]
        a = a
        [a] = [a]
        f = 1
        st' :: 2 = 7
        x :: a = a
        rhs1 :: [a]

accumulate :: 1 -> 2 -> 3
    f :: 1 = ?
    st :: b = ?
    rhs :: [a] = ?
    let (*) in (**)
    (*): 
        (f @ st) :: (b -> (a,b)) -> b -> (a,b)
        f :: (b -> (a,b))
        st :: b = b
        (x,st') :: (a,b)
        (a,b) = (a,b)
    (**):
        (accumulate @ f @ st') :: (1 -> b -> [a]) -> 1 -> b -> [a]
        x :: a
        (:) :: a -> [a] -> [a]
        a = a
        [a] = [a]
        f :: 1 = (b -> (a,b))
        st' :: b = b
        x :: a = a
        rhs1 :: [a]

accumulate :: 1 -> 2 -> 3
    f :: (b -> (a,b)) = ?
    st :: b = ?
    rhs :: [a] = ?
    let (*) in (**)
    (*): 
        (f @ st) :: (b -> (a,b)) -> b -> (a,b)
        f :: (b -> (a,b))
        st :: b = b
        (x,st') :: (a,b)
        (a,b) = (a,b)
    (**):
        (accumulate @ f @ st') :: ((b -> (a,b)) -> b -> [a]) -> (b -> (a,b)) -> b -> [a]
        x :: a
        (:) :: a -> [a] -> [a]
        a = a
        [a] = [a]
        f :: (b -> (a,b)) = (b -> (a,b))
        st' :: b = b
        x :: a = a
        rhs1 :: [a]

accumulate :: (b -> (a,b)) -> b -> [a]

-}