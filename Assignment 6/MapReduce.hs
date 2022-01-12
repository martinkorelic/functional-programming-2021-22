module MapReduce where

import Data.List
import Data.Monoid
import Data.Function
import OrdList

foldm :: (a -> a -> a) -> a -> ([a] -> a)
foldm f a [x] = f a x
foldm f a x = let s = length x `div` 2 in foldm f a (take s x) `f` foldm f a (drop s x)

foldm' :: (a -> a -> a) -> a -> ([a] -> a)
foldm' f a [x] = f a x 
foldm' f a x = foldm' f a (zipWith f h1 h2)
  where
    s = length x `div` 2
    h1 = take s x
    h2 = drop s x

{- Some benchmarking code for your foldm/foldm' implementation -}

type Folder a = (a -> a -> a) -> a -> ([a] -> a)

test :: (Monoid a) => (Int -> a) -> Folder a -> Int -> a
test toMonoid fold n = (reduce . map toMonoid) (take n zx81)
  where
  reduce = fold (<>) mempty

  -- a humble Lehmer random number generator;
  -- don't implement your next cryptosystem with it
  zx81 :: [Int]
  zx81 = unfoldr (\r->Just(r `mod` 65537,(r+1)*75-1)) (n*0xcafe)

bench :: (Show a, Monoid a) => (Int -> a) -> Folder a -> Int -> ()
bench toMonoid fold n = omnomnom (show $ test toMonoid fold n)
  where
  -- this may look like a pretty bizarre function, which does nothing except traverse the list.
  -- but, it forces strict evaluation of all list contents, making it very useful for benchmarking!
  -- (note: real world benchmarking inside GHCi is pretty pointless, as it doesn't optimize your code)
  omnomnom :: [a] -> ()
  omnomnom [] = ()
  omnomnom (x:xs) = x `seq` omnomnom xs
