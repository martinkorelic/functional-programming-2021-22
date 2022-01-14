
----------------------------------------------------
To prove: map (f . g) xs = map f (map g xs)
By induction on xs.

Case 1: xs = []

    map (f . g) []

  = []

  = map f []

  = map f (map g [])

Case 2: xs = (a:as)
IH: map (f . g) as = map f (map g as), for all f and g

    map (f . g) (a:as)

  = map (\x -> f (g x)) (a:as)
  { def. map }
  = f (g a) : map (f . g) as
  { IH }
  = f (g a) : map f (map g as)
  { def. map }
  = map f (g a:(map g as))
  { def. map }
  = map f (map g (a:as))


-----------------------------------------------------
To prove: map f (as ++ bs) = (map f as) ++ (map f bs)
By induction on bs

Case 1: as = []

  map f ([] ++ bs)
  =
  map f bs
  =
  [] ++ map f bs
  =
  (map f []) ++ (map f bs)

Case 2: as = x:xs
IH: map f (xs ++ bs) = (map f xs) ++ (map f bs)

map f ((x:xs) ++ bs)
=
map f (([x] ++ xs) ++ bs)
=
map f ([x] ++ (xs ++ bs))
=
map f (x:(xs ++ bs))
=
f x : map f (xs ++ bs)
=
(f x : (map f xs)) ++ (map f bs)
=
(map f (x:xs)) ++ (map f bs)

-----------------------------------------------------
To prove: concat (map (map f) xs) = map f (concat xs)


Case 1: xs = []

  concat (map (map f) []) 
  =
  concat []
  =
  []
  =
  map f []
  = 
  map f (concat [])

Case 2: xs = (a:as)
IH: concat (map (map f) as) = map f (concat as)

concat (map (map f) (a:as))
=
concat ((map f a):(map (map f) as))
=
(map f a) ++ concat (map (map f) as)
=
(map f a) ++ map f (concat as)
=
map f (a ++ concat as)
=
map f (concat (a:as))
