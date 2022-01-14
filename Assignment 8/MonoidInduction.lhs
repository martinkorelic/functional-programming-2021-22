> module MonoidInduction where
> import Prelude hiding (foldl,foldr,mconcat)
>
> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr _ b [] = b
> foldr f b (x:xs) = f x (foldr f b xs)

> foldl :: (b -> a -> b) -> b -> [a] -> b
> foldl _ b [] = b
> foldl f b (x:xs) = foldl f (f b x) xs
>
> mconcat :: (Monoid a) => [a] -> a
> mconcat = foldr (<>) mempty

-------------------------------------------------------
To prove: mconcat (xs ++ ys) = mconcat xs <> mconcat ys

Case 1: xs = [] 

  mconcat ([] ++ ys) 
          ----------  defn. of ++ 
= mconcat ys
 ^                    monoid laws
= mempty <> mconcat ys
  ------              defn. of foldr on empty list 
= foldr (<>) mempty [] <> mconcat ys
  -------------------- defn. of mconcat 
= mconcat [] <> mconcat ys 

Case 2: xs = a:as 
IH: mconcat (as ++ ys) = mconcat as <> mconcat ys for all ys
  
  mconcat ((a:as)++ys) 
  -------                        defn of mconcat 
= foldr (<>) mempty ((a:as)++ys)
                    ------------ defn of ++
= foldr (<>) mempty (a:as++ys)
  ----------------------------   defn of foldr
= a <> foldr (<>) mempty (as++ys) 
       -----------------         defn of mconcat
= a <> mconcat (as++ys)
       ---------------- IH 
= a <> (mconcat as <> mconcat ys)
  -------------------------------  rearranging by associativity of <> 
= (a <> mconcat as) <> mconcat ys 
        -------defn of mconcat 
= (a <> foldr (<>) mempty as) <> mconcat ys 
  --------------------------defn of foldr
= (foldr (<>) mempty (a:as) <> mconcat ys 
  ------------------ defn of mconcat 
= mconcat (a:as) <> mconcat ys

-------------------------------------------------------
To prove: foldl (<>) (x<>y) xs = x <> foldl (<>) y xs

Case 1: xs = []

  foldl (<>) (x<>y) []
  --------------------      definition of foldl
= x <> y  
       -                    definition of foldl 
= x <> foldl (<>) y []

Case 2: xs = (a:as) 
IH: foldl (<>) (x<>y) as = x <> foldl (<>) y as, for all x,y 

  foldl (<>) (x<>y) (a:as) 
  -----------------------   definition of foldl 
= foldl (<>) ((x<>y)<>a) as 
             ----------     associativity of <>
= foldl (<>) (x<>(y<>a)) as 
  ------------------------- induction hypothesis (with x:=x & y:=y<>a)
= x <> foldl (<>) (y<>a) as 
       -------------------- definition of foldl
= x <> foldl (<>) y (a:as) 

-------------------------------------------------------
To prove: foldl (<>) mempty xs = foldr (<>) mempty xs

Case 1: xs = []

  foldl (<>) mempty []
  -------------------- definition of foldl 
= []
  -- definition of foldr 
= foldr (<>) mempty [] 

Case 2: xs = (a:as)
IH: foldl (<>) mempty as = foldr (<>) mempty as 

  foldl (<>) mempty (a:as)
  ------------------------  definition of foldr 
= foldl (<>) (mempty<>a) as 
             -----------    identity-law for monoids
= foldl (<>) (a<>mempty) as  
  ------------------------- by the previous proof 
= a <> foldl (<>) mempty as
       -------------------- IH 
= a <> foldr (<>) mempty as 
  ------------------------- definition of foldr 
= foldr (<>) mempty (a:as) 

QED