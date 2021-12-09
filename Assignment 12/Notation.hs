module Notation where

import Data.Time

siri :: IO ()
siri = 
  putStrLn "What is your name?" >>
  getLine >>= \name ->
  getZonedTime >>= \now ->
  putStrLn (name ++ formatTime defaultTimeLocale ", the time is %H:%M" now)

siri' :: IO [Char]
siri' = do
  out <- putStrLn "What is your name?"
  name <- getLine
  time <- getZonedTime
  return (name ++ formatTime defaultTimeLocale ", the time is %H:%M" time)

mayLookup :: (Eq a) => Maybe a -> [(a, b)] -> Maybe b
mayLookup maybekey assocs = do
  key <- maybekey
  result <- lookup key assocs
  return result

mayLookup' :: (Eq a) => Maybe a -> [(a, b)] -> Maybe b
mayLookup' maybekey assocs = 
  maybekey >>= \k ->
  lookup k assocs

-- Gets a key value from the "maybekey" and then passes it onto lookup function that
-- looks up the value in the association list