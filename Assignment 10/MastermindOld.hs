module Mastermind
where
import System.Random
import Data.Char

dice :: IO Int
dice = getStdRandom (randomR (1,6))

roll :: IO Int
roll  =  do  a <- dice
             b <- dice
             return (a + b)

randomWord :: Int -> Int -> IO([Int])
randomWord 0 y = return []
randomWord x y = do
  r <- getStdRandom (randomR(1,y))
  rs <- randomWord (x-1) y
  return (r:rs)
  
code :: Int -> IO Int
code x = getStdRandom (randomR (1,x))

-- Call this to start the game!
gameStart :: IO ()
gameStart = do
  putStrLn ("How many colors?:")
  a <- getLine
  putStrLn ("How long is the code?:")
  b <- getLine
  putStrLn ("Number of guesses?:")
  c <- getLine
  compareWord (head (makeWord c)) (randomWord (head (makeWord b)) (head (makeWord a)))

printMe :: IO([Int])
printMe = do
  putStrLn ("Guess the code:")
  answer <- getLine
  return (makeWord answer)
  

compareWord :: Int -> IO([Int]) -> IO ()
compareWord x a = do
  secret <- a
  compareWord1 (x-1) secret
   
compareWord1 :: Int -> [Int] -> IO ()
compareWord1 x a = do
  b <- printMe
  c <- (checkWordIO b a)
  if (and [correct c, (isCorrectLength a c)]) then do
    putStrLn("Congratulations! The code was:")
    print a
  else 
   if (x==0) then do
    putStrLn ("Out of guesses. The answer is:")
    print a
   else do
    putStrLn ("Wrong guess. Hint is :")
    print c
--  putStrLn ("Answer")
--  print a
    compareWord1 (x-1) a

correct :: [Int] -> Bool
correct [] = True
correct (a:as)
  | a == 2 = correct as
  | otherwise = False

checkWordIO :: [Int] -> [Int] -> IO [Int]
checkWordIO [] [] = return []
checkWordIO a b = return (checkWord 1 a b)
  
checkWord :: Int -> [Int] -> [Int] -> [Int]
checkWord i [] [] = []
checkWord i [] b = []
checkWord i (a:as) b
  | isCorrectSpot a i b = 2 : checkWord (i+1) as b
  | isMember a b = 1 : checkWord (i+1) as b
  | otherwise = 0 : checkWord (i+1) as b

  
isCorrectSpot :: Int -> Int -> [Int] -> Bool
isCorrectSpot x i [] = False
isCorrectSpot x i (y:ys)
  | i > 1 = isCorrectSpot x (i-1) ys
  | otherwise = x == y

isMember :: Int -> [Int] -> Bool
isMember x [] = False
isMember x (y:ys)
  | x == y = True
  | otherwise = isMember x ys

isCorrectLength :: [Int] -> [Int] -> Bool
isCorrectLength a b = (length a) == (length b)

makeWord :: String -> [Int]
makeWord s = map (read) (words s)