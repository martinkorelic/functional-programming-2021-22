module Obfuscate where

import Data.Char
import Data.List


meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."

removeAt :: Int -> [a] -> [a]
removeAt n xs = [ x | (i,x) <- zip [0..(length xs)] xs, i /= n]

jumble :: [a] -> [a]
jumble s
       | null s = s
       | otherwise = s !! rn : jumble (removeAt rn s) where rn = 13 `rem` length s

wordJumble :: [Char] -> [Char]
wordJumble s
       | all isDigit s = s
       | length s < 2 = s
       | isPunctuation (last s) = wordJumble (head s : init (tail s)) ++ [last s]
       | otherwise = [head s] ++ jumble (init (tail s)) ++ [last s]

cambridge :: String -> String
cambridge s = unwords (map wordJumble (words s))