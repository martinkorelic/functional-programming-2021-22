module Reverse where

import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

reverse' :: [a] -> [a]
reverse' xs = rev xs []
  where rev []     acc = acc
        rev (y:ys) acc = rev ys (y:acc)

{-

reverse' takes less time to perform.
reverse is more easier to write and understand from code.
reverse is still preferred.

-}