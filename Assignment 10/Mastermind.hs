module Main where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import System.IO
import Data.Int (Int)
import Data.Sequence.Internal.Sorting (QList(Nil))

{---------------------- functional parts -------------------------------}

data Colour = White | Silver | Green | Red | Orange | Pink | Yellow | Blue
  deriving (Eq, Show, Ord, Read)

scoreAttempt :: (Ord a) => [a] -> [a] -> (Int, Int)
scoreAttempt code guess = (a, b-a) where
  a = correctRightPos code guess
  b = correctWrongPos code guess

correctRightPos :: (Ord a) => [a] -> [a] -> Int
correctRightPos code guess = sum (zipWith (\ a b -> (if a == b then 1 else 0)) code guess)

correctWrongPos :: (Ord a) => [a] -> [a] -> Int
correctWrongPos code guess = length code - length (removeElems code guess)

removeElems :: (Ord a) => [a] -> [a] -> [a]
removeElems = foldl (flip delete)

-- Some test cases from: https://www.boardgamecapital.com/game_rules/mastermind.pdf
test1, test2, test3, test4 :: Bool
test1 = scoreAttempt [1,2,3,4,5 :: Int] [2,6,3,7,8 :: Int] == (1,1)
test2 = scoreAttempt [1,2,3,4,2 :: Int] [5,6,3,3,7 :: Int] == (1,0)
test3 = scoreAttempt [1,2,1,3,3 :: Int] [4,1,5,6,7 :: Int] == (0,1)
test4 = scoreAttempt [4,1,5,6,7 :: Int] [1,2,1,3,3 :: Int] == (0,1)

{---------------------- IO parts -------------------------------}

-- only here for example; you can remove these from your final file
roll_d6 :: IO Int
roll_d6 = randomRIO (1,6)

roll_2d6 :: IO Int
roll_2d6 = do
  a <- roll_d6
  b <- roll_d6
  return (a + b)

getCode :: Int -> IO [Colour]
getCode n = do
  randomNumbers <- randomList n
  return (map giveColour randomNumbers)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
  r <- randomRIO (0,7)
  rs <- randomList (n-1)
  return (r:rs)

giveColour :: Int -> Colour
giveColour 0 = White
giveColour 1 = Silver
giveColour 2 = Green
giveColour 3 = Red
giveColour 4 = Orange
giveColour 5 = Pink
giveColour 6 = Yellow
giveColour _ = Blue

playGame :: (Ord a, Read a, Show a) => Int -> [a] -> IO ()
playGame 0 x = do
  putStrLn "No more tries, game over."
  putStrLn ("The code was " ++ show x)
playGame attempt x = do
  putStrLn ("\nTry to guess the secret code word, " ++ show attempt ++ " tries left")
  try <- gameAttempt x
  if try then putStrLn "Correct" else playGame (attempt - 1) x

gameAttempt :: (Ord a, Read a) => [a] -> IO Bool
gameAttempt x = do
  guess <- getLine
  let (a, b) =  scoreAttempt x (map read (words guess))
  if a == length x then
    return True
  else do
    putStrLn "Incorrect"
    putStrLn(show a ++ " colour(s) in the correct position,")
    putStrLn(show b ++ " colour(s) in the wrong position.")
    return False

main :: IO ()
main = do
  putStrLn "What should be the length of the code?"
  c <- getLine
  code <- getCode (read c)
  putStrLn "How many guesses would you like?"
  n <- getLine
  putStrLn ("\n\nI picked a random code word with " ++ show c ++ " colours.")
  putStrLn "Possible colours are White Silver Green Red Orange Pink Yellow Blue."
  playGame (read n) code
