module Uniq where

import Data.List

uniq :: (Eq a) => [a] -> [a]
uniq s = map head (group s)