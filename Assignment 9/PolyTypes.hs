module PolyTypes where

mingle xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

sumWith g xs = foldr (+) 0 (map g xs)

transform f  = concat . map f
