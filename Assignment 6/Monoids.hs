module Monoids where

{- Determine if the following operations turn a type into a Monoid;
 - if so: give the corresponding value for the identity element

   The list cons operator,   (:) :: a -> [a] -> [a]

   Yes,
   a:(b:c) = (a:b):c

   mempty = []

   The boolean operator      (||) :: Bool -> Bool -> Bool

    Yes,
    a || (b || c) = (a || b) || c
    False || a =  a

    mempty = False

   The function              mod :: (Integral a) => a -> a -> a

    No,
    a mod (b mod c) != (a mod b) mod c

   The function              max :: (Ord a) => a -> a -> a
  
    Yes,
    a `max` (b `max` c) = (a `max` b) `max` c

    mempty = 0

   Function composition,     (.) :: (b -> c) -> (a -> b) -> (a -> c)

    Yes,
    a . (b . c) = (a . b) . c

    mempty = id

   The function              zipWith (+) :: [Int] -> [Int] -> [Int]

    Yes,
    zipWith (+) a (zipWith (+) b c) = zipWith (+) c (zipWith (+) a b)

    mempty = [0..]

   The operator              (++/) :: [a] -> [a] -> [a] defined below

    No,
    a ++/ (b ++/ c) = (a ++/ b) ++/ c

  -}

(++/) :: [a] -> [a] -> [a]
[]     ++/ ys = ys
(x:xs) ++/ ys = x:(ys ++/ xs)