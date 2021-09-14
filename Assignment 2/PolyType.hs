module PolyType where

f8 :: Ord p => p -> p -> p
f8 x y  = if x <= y then x else y

f9 x y  = not x || y

f10 x y
  | x == 0    = y
  | otherwise = x + y

f11 x y = get 0
  where get n = if n == 0 then x else y

--2.4.1
--f8 and f11 can be used on arguments of type String
--
--2.4.2
--f8 is ad-hoc polymorphic
--f9 is not polymorphic, the type has to be a Bool
--f10 is ad-hoc polymorphic
--f11 is parametric polymorphic