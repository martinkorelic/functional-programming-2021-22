module ShortCircuit where

andl, andr, orl, orr :: [Bool] -> Bool

andl = foldl (&&) True
andr = foldr (&&) True
orl  = foldl (||) False
orr  = foldr (||) False

e1, e2, e3, e4 :: Bool

e1 = andl $ False : [True,  True  ..] -- infinite
e2 = andr $ False : [True,  True  ..] -- False
e3 = orl  $ True  : [False, False ..] -- infinite
e4 = orr  $ True  : [False, False ..] -- True

-- foldr, because we're folding from the start of the list to the end