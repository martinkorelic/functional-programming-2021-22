module Strict where

import Data.List

{-

1. seq first reduces first argument and then second.
For foldl intermediate sums are evaluated after it is traversed through the whole list.
For foldl' intermediate sums are evaluated immediately, followed by other sums.

2. foldr is more efficient.

3. foldl' as it can reduce the boolean values faster and therefore shortcircuit faster.

-}