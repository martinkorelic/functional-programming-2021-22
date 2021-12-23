{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Instances where

import Result

-- you should only implement *either* foldr or foldMap; doing both is unnecessary
instance Foldable Result where
  foldr :: (a -> b -> b) -> b -> Result a -> b
  foldr f acc (Error msg) = acc
  foldr f acc (Okay a) = f a acc
  --foldMap  :: (Monoid m) => (a -> m) -> Result a -> m

instance Traversable Result where
  traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error msg) = pure (Error msg)
  traverse f (Okay x)    = Okay <$> f x

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq,Show)

instance Foldable Tree where
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty
  foldMap f (Node x a b) = f x <> foldMap f a <> foldMap f b

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node x a b) = Node (f x) (fmap f a) (fmap f b)

instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Node x a b) = Node <$> f x <*> traverse f a <*> traverse f b

assistants :: Tree String
assistants = Node "Patrick" 
               (Node "Jen" 
                  (Node "Cassian" (Node "Bram" Leaf Leaf) Leaf) 
                  (Node "Mario" Leaf Leaf))
               (Node "Sander" 
                  (Node "Rico" (Node "Quinten" Leaf Leaf) Leaf)
                  (Node "Willem" Leaf Leaf))

flatten :: (Foldable t) => t String  -> String
flatten = foldr1 (\x y->x++", "++y)
