module Folds where

import Data.List
import Data.Function

mySum :: (Foldable f, Num a) => f a -> a
mySum = foldr (+) 0

myLength :: (Foldable f) => f a -> Int
myLength = foldr (\_ a -> a+1) 0

slowMean :: (Foldable t) => t Integer -> Double
slowMean xs = fromIntegral (mySum xs) / fromIntegral (myLength xs)

myMean :: (Foldable t) => t Integer -> Double
myMean xs = fromIntegral s / fromIntegral len
    where
        (s,len) = foldr (\x (i,l) -> (i+x,l+1)) (0,0) xs

bigList :: [Integer]
bigList = take 100000 $ unfoldr (Just . (\x->(x `mod` 10+1,(75*(x+1)-1) `mod` 0x10001))) 37