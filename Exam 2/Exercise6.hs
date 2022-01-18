module Exercise6 where

data Tree elem      = Empty | Node (Tree elem) elem (Tree elem)
data TREE elem tree = EMPTY | NODE tree elem tree

instance Functor (TREE elem) where
    fmap f EMPTY = EMPTY
    fmap f (NODE l a r) = NODE (f l) a (f r)

instance Base (TREE elem) where
    type Rec (TREE elem) = Tree elem
    inn EMPTY            = Empty
    inn (NODE l a r)     = Node l a r
    out Empty            = EMPTY
    out (Node l a r)     = NODE l a r

fold :: (Base f) => (f a -> a) -> (Rec f -> a)
fold alg = consume
  where consume = alg . fmap consume . out

unfold :: (Base f) => (a -> f a) -> (a -> Rec f)
unfold coalg = produce
  where produce = inn . fmap produce . coalg

size :: Tree elem -> Integer
size = fold (\x -> case x of
                     EMPTY        -> 0
                     NODE sl a sr -> sl + 1 + sr)


print' :: (Show elem) => Tree elem -> String
print' = fold (\x -> case x of
                       EMPTY        -> "."
                       NODE sl a sr -> "(" ++ sl ++ ") " ++ show a ++ " (" ++ sr ++ ")")

distribute :: [elem] -> Tree elem
distribute = unfold (\values -> case values of
                                  []    -> EMPTY
                                  [x]   -> NODE [] x []
                                  more  -> NODE as (head bs) (tail bs)
                                    where n = length more
                                          (as,bs) = splitAt (div n 2) more
           )