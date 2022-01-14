module Main where 

import System.Random 
import System.Environment 
import Data.List 
import Data.Char

-- this solution tries to "minimize" the IO content of the program, by 
-- 1. first creating a (lazy) infinite list of random ints
-- 2. (using IO) reading file contents 
-- 3. applying the encryption purely functionally 
-- 4. (using OI) writing the output 

main :: IO () 
main = do
  let generator = mkStdGen 1377
  let keypad = unfoldr (Just . random) generator 

  [cmd,inf,outf] <- getArgs 
  plainText <- readFile inf
  let cipherText = zipWith (caesar $ if cmd=="encrypt" then (+) else (-)) keypad plainText 
  writeFile outf cipherText 

caesar :: (Int -> Int -> Int) -> Int -> Char -> Char
caesar op r = chr . encrypt . ord 
  where 
  encrypt a 
    | a < 32     = a
    | otherwise  = ((a-32 `op` r) `mod` (128-32)) + 32 