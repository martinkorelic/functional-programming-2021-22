module RandomGen where

import Control.Monad
import System.Random
import RandomState

genRandIntegerIO :: (Integer,Integer) -> IO Integer
genRandIntegerIO bounds = do
  x <- getStdGen
  let (y, gen) = randomR bounds x
  setStdGen gen
  return y

genRandInteger :: (Integer,Integer) -> RandomState Integer
genRandInteger (a,b) = do
  x <- get
  let (y, gen) = randomR (a,b) x
  put gen
  return y

roll_2d6 :: RandomState Integer
roll_2d6 = do
  a <- genRandInteger (1,6)
  b <- genRandInteger (1,6)
  return (a+b)

safeR :: RandomState a -> IO a
safeR m = do
  x <- getStdGen
  let (i, st) = runState m x
  setStdGen st
  return i

--these definitions can be used to test your function a bit more thoroughly
randomN :: (Integer,Integer) -> Int -> StdGen -> [Integer]
randomN (a,b) n g = result
  where (result, _) = runState (replicateM n (genRandInteger (a,b))) g

testme :: [Integer]
testme = randomN (0,999) 100 (mkStdGen 42)
