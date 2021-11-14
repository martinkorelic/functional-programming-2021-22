> module FoldrFusion where
>
> import Prelude hiding (map)

The fusion law for foldr states that

IF, for all x,y:
  f (g x y) = h x (f y)
THEN
  f . foldr g e = foldr h (f e)

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (\x xs -> f x : xs) []

----------------------------------------------
To prove:  foldr g e . map f = foldr (g . f) e

To show that foldr g e . map f = foldr (g . f) e, we can apply the fusion law using
  f ==> foldr g e
  g ==> \x xs -> f x : xs
  e ==> []
  h ==> g . f

Namely as follows:

  foldr g e . map f
              ----- rewrite map as foldr
= foldr g e . foldr (\x xs->f x : xs) []
  ---------------------------------------- foldr fusion
= foldr (g . f) (foldr (\x xs->f x : xs) e [])
                ------------------------------ definition of foldr
= foldr (g . f) e

Since the "THEN" part of the fusion law can only be applied if the "IF" part is true,
we need to show that ..  

f (g x y) = h x (f y)

Which is the case since:

f (g x y)
= { sub. for f }
foldr g e (g x y)
= { sub. for g }
foldr (\x xs -> f x : xs) e (\x xs -> f x : xs (x) (y))
= { application of lambda }
foldr (\x xs -> f x : xs) e (f x : y)
= { def. of foldr }
(\x xs -> f x : xs) (f x) (foldr (\x xs -> f x : xs) e y)
= { sub. for g }
g (f x) (foldr (\x xs -> f x : xs) e y)
= { def. of (.) }
g . f x (foldr (\x xs -> f x : xs) e y) 
= { sub. for g }
g . f x (foldr g e y)
= { sub. for h }
h x (foldr g e y)
= { sub. for f }
h x (f y)

--------------------------------------
To prove:  map (f . g) = map f . map g

IF, for all x,y:
  f (g x y) = h x (f y)
THEN
  f . foldr g e = foldr h (f e)


THEN part:

map f . map g
=
map f . foldr (\x xs -> g x : xs) []

=>
f = map f
g = \x xs -> g x : xs
e = []
h = \x xs -> f (g x) : xs

=>
foldr h (f e)
= { sub. for h }
foldr (\x xs -> f (g x) : xs) (map f e)
= { sub. for e }
foldr (\x xs -> f (g x) : xs) []
= { def. of map }
foldr (\x xs -> f (g x) : xs) []
= { def. of (.) }
foldr ((\x xs -> f x : xs) . g) []
= { def. of map }
map (f . g)



IF part:
f (g x y) = h x (f y)


f (g x y)
= { sub. for f }
map f (g x y)
= { sub. for g }
map f (\x xs -> g x : xs (x) (y))
= { apl. of lambda }
map f (g x : y)
= { def. of map }
f (g x) : map f y
= { lambda expression }
(\x xs -> f (g x) : xs) (x) (map f y)
= { sub. for h }
h x (map f y)
= { sub. for f }
h x (f y)


----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

