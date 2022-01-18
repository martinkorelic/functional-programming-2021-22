module Exercise2 where

import Data.List

newtype CircList a = CL { fromCL:: [a] }
    deriving (Show)

size :: CircList a -> Int
size (CL a) = length a

current :: CircList a -> Maybe a
current (CL a) = if size (CL a) == 0 then Nothing else Just (head a)

insert :: [a] -> CircList a -> CircList a
insert ls (CL a) = CL $ ls ++ a

delete :: Int -> CircList a -> CircList a
delete i (CL a) = CL $ drop i a

rotate :: Int -> CircList a -> CircList a
rotate i (CL a) = CL $ drop i a ++ take i a

takeFrom :: Int -> CircList a -> CircList a
takeFrom i (CL a) = CL $ concat (replicate (i `div` length a) a) ++ take (i `mod` length a) a

equalCL :: (Eq a) => CircList a -> CircList a -> Bool
equalCL (CL a) (CL b) = if length a /= length b then False else equal' (length a) (CL a) (CL b)

equal' :: (Eq a) => Int -> CircList a -> CircList a -> Bool
equal' 0 (CL a) (CL b) = a == b
equal' i a b = fromCL a == fromCL (rotate i b) || equal' (i-1) a b