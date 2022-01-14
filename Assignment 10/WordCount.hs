-- ghc --make WordCount.hs
module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  --print args
  --or, to get "UNIX cat":
  fileContents <- mapM readFile args
  let h = zipWith (\x y -> unwords (map show $ wcCount x) ++ " " ++ x ++ "\n") args fileContents
  mapM_ putStr h

wcCount :: String -> [Int]
wcCount str = [length (lines str), length (words str), length str]