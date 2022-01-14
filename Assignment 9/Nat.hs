module Nat where

data Nat = O | S Nat
  deriving Show

instance Eq Nat where
  (==) n1 n2 = fromNat n1 == fromNat n2

instance Ord Nat where
  (<=) n1 n2 = fromNat n1 <= fromNat n2

toNat :: Integer -> Nat
toNat n | n < 0 = error "can't convert negative values to Nat"
toNat 0 = O
toNat n = S (toNat (n-1))

-- "canned recursion" on Nat's
natFoldr :: (a -> a) -> a -> Nat -> a
natFoldr f b = go
  where
  go O     = b
  go (S m) = f (go m)

fromNat :: (Integral n) => Nat -> n
fromNat = natFoldr (+1) 0

infinity :: Nat
infinity = S infinity
-- infinite recursion

(+.) :: Nat -> Nat -> Nat
(+.) n1 n2 = toNat (if r < 0 then 0 else r)
  where
    r = fromNat n1 + fromNat n2

(-.) :: Nat -> Nat -> Nat
(-.) n1 n2 = toNat (if r < 0 then 0 else r)
  where
    r = fromNat n1 - fromNat n2

natLength :: [a] -> Nat
natLength = toNat . fromIntegral . length
