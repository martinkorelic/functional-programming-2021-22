module Exercise4 where

type Hash = [Int]
hashInt :: Int -> Hash
hashCombine :: Hash -> Hash -> Hash

class Hashable a where
    hash :: a -> Hash

instance Hashable Int where
    hash = hashInt

instance (Hashable a, Hashable b) => Hashable (Either a b) where
    hash (Left x) = hashInt 0 `hashCombine` hashInt x
    hash (Right y) = hashInt 0 `hashCombine` hashInt y

instance (Hashable a, Hashable b) => Hashable (a,b) where
    hash (x, y) = hashInt x `hashCombine` hashInt y

hashContainer :: (Foldable t, Hashable a) => t a -> Hash
hashContainer ls = foldr (\x a -> hashInt x `hashCombine` a) (hashInt 0)

data WithHash a = WithHash a Hash

withHash :: (Hashable a) => a -> WithHash a
withHash h = WithHash h (hash h)

instance (Eq a) => Eq (WithHash a) where
    WithHash x y == WithHash x' y' = y == y' && x == x'

{-
Can this WithHash type be made an instance of `Traversable`, while making sure that the
stored hash code is always correct? Motivate your answer.

No, traverse is polymorphic over stored type a, and doesn’t allow for a Hash-
able constraint.

-}