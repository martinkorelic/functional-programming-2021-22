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
  
  foldr g e ((\x xs -> f x : xs) x y) = (g . f) x (foldr g e y)

Which is the case since:

  foldr g e ((\x xs -> f x : xs) x y)
  = { lambda }
  foldr g e (f x : y)
  = { def. foldr }
  g (f x) (foldr g e y)
  = { def. (.) }
  ((g . f) x) (foldr g e y)
  =
  (g . f) x (foldr g e y)

--------------------------------------
To prove:  map (f . g) = map f . map g

  map (f . g)
  = { map using foldr }
  foldr (\x xs -> (f . g) x : xs) []
  = { def. (.) }
  foldr (\x xs -> f (g x) : xs) []
  = { lambda comp. }
  foldr ((\y xs -> f y: xs) . g) []
  = { foldr-map fusion }
  foldr (\y xs -> f y : xs) [] . map g
  = { def. map }
  map f . map g

----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat


note that concat can be written as: 
concat = foldr (++) []

     mconcat . concat 
   =           ------ definition of concat
     mconcat . foldr (++) []
   = ----------------------- foldr fusion
     foldr h (mconcat [])
      where mconcat (x ++ y) = h x (mconcat y)   for all x,y  [see lemma] 
      so h = ((<>) . mconcat), and we get:
     foldr ((<>) . mconcat) (mconcat [])
                            ------------ definition of mconcat 
   = foldr ((<>) . mconcat) (foldr (<>) mempty []) 
                            ---------------------- foldr of empty list
     foldr ((<>) . mconcat) mempty
   = ----------------------------- by map fusion
     foldr (<>) mempty . map mconcat 
   = ----------------- definition of mconcat 
     mconcat . map mconcat 

We can also prove the lemma mconcat (x ++ y) = mconcat x <> mconcat y for all x,y using fusion laws:

note that: (++) can be written as
(++) x y = foldr (:) y x 

Proof:

    mconcat (x ++ y)
  =         -------- definition of (++) 
    mconcat (foldr (:) y x) 
  = ----------------------- foldr fusion 
    foldr (<>) (mconcat y) x
      where mconcat (u:v) = u <> mconcat v  for all u,v,
      by definition of mconcat and foldr 
  =            ----------- monoid law 
    foldr (<>) (mempty <> mconcat y) x 
  = ---------------------------------- foldr fusion (reverse) 
    (<> mconcat y) (foldr (<>) mempty x)
      where ((u <> v) <> mconcat y) = u <> (v <> mconcat y)
      by monoid law
  =                --------------------- definition of mconcat 
    (<> mconcat y) mconcat y
  = ------------------------ rewrite in infix style
    mconcat x <> mconcat y