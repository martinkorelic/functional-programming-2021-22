module Demo where

import qualified Data.Map as Map

expr1,expr2 :: Maybe Bool
expr3 :: Maybe a
expr4 :: Maybe Int

expr1 = Just True
expr2 = Just False
expr3 = Nothing
expr4 = Just 42

expr5 :: [a]
expr6 :: [Int]
expr7 :: [Bool]

expr5 = []
expr6 = [1..5]
expr7 = [True,True,False]

expr8, expr9 :: Map.Map String Double
expr8 = Map.empty
expr9 = Map.fromList [("Rinus", 7.5), ("Peter", 8.2), ("Ralf", 6.8)]

{-

1.
expr1: Error, True, True

expr2: Error, True, False

expr3: 0, True, False

expr4: 42, Error, Error

expr5: 0, True, False

expr6: 15, Error, Error

expr7: Error, False, True

expr8: 0.0, Error, Error

expr9: 22.5, Error, Error

2.
Traverses if the type is numeric and replaces with Maybe type,
Prints expressions for each type,
Replaces data and prints.

-}