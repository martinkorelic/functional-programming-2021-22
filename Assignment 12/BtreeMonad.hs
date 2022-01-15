module BtreeMonad where

data Btree a = Tip a | Bin (Btree a) (Btree a)
  deriving Show

instance Functor Btree where
  --fmap :: (a -> b) -> Btree a -> Btree b
  fmap f (Tip x)   = Tip (f x)
  fmap f (Bin l r) = Bin (fmap f l) (fmap f r)

instance Applicative Btree where
  pure x = Tip x

  (Tip f) <*> tree = fmap f tree
  (Bin fl rl) <*> tree = Bin (fl <*> tree) (rl <*> tree)

instance Monad Btree where
  return x = Tip x

  Tip x >>= f = f x
  Bin l r >>= f = Bin (l >>= f) (r >>= f) 