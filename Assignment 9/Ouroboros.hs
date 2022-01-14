module Ouroboros where

import Data.Maybe

type AssocList k a = [(k,a)]

insertLookup :: (Eq k) => k -> a -> AssocList k a -> (Maybe a, AssocList k a)
insertLookup k v l = if isNothing lk then (Nothing, l ++ [(k,v)]) else (lk, l)
    where
        lk = lookup k l

insert :: (Eq k) => k -> a -> AssocList k a -> AssocList k a
insert k v kvs = kvs'
  where (_, kvs') = insertLookup k v kvs

lookup1 :: (Eq k) => k -> AssocList k a -> Maybe a
lookup1 k kvs   = x
  where (x, _) = insertLookup k undefined kvs

adjust :: (Eq k) => (a -> a) -> k -> AssocList k a -> AssocList k a
adjust f k l = if isNothing lk then l else map (\(kl,v) -> if k==kl then (kl,f v) else (kl,v)) l
    where
        lk = lookup k l