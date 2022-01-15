module AskNames where

generateNames :: [String] -> [String] -> [String]
generateNames firstnames surnames = [ f ++ " " ++ l | f <- firstnames, l <- surnames ]

generateNames' :: Applicative f => f [Char] -> f [Char] -> f [Char]
generateNames' firstnames surnames = (\x y -> x ++ " " ++ y) <$> firstnames <*> surnames

getFullName :: IO String
getFullName = do
  first   <- putStrLn "First name?" >> getLine
  surname <- putStrLn "Last name?"  >> getLine
  return (first ++ " " ++ surname)

getFullName' :: IO [Char]
getFullName' = (\x y -> x ++ " " ++ y) <$> (putStrLn "First name?" >> getLine) <*> (putStrLn "Last name?" >> getLine)

makeName :: (Applicative f) => f String -> f String -> f String
makeName first surname = pure (\f l->f ++ " " ++ l) <*> first <*> surname

generateNames'' :: Applicative f => f String -> f String -> f String
generateNames'' = makeName

getFullName'' = makeName (putStrLn "First name?" >> getLine) (putStrLn "Last name?" >> getLine)