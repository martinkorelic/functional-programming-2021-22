module FMapExpr where

import Data.Char

-- Increments a list by 1
-- Functor []
-- (a -> b) -> [a] -> [b]
expr1 = fmap (\x->x+1) [1,2,3]

-- Appends "dr." to functor
-- Functor Maybe
-- (a -> b) -> Maybe a -> Maybe b
expr2 = fmap ("dr." ++) (Just "Sjaak")

-- String to lower
-- Functor []
-- (a -> b) -> [a] -> [b]
expr3 = fmap toLower "Marc Schoolderman"

-- Applies "dr." to each element Maybe in a list
-- Functor [] and Maybe
-- (Maybe [Char] -> Maybe [Char]) -> [Maybe [Char]] -> [Maybe [Char]]
expr4 = fmap (fmap ("dr." ++)) [Nothing, Just "Marc", Just "Twan"]