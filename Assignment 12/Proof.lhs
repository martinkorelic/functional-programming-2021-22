Exercise: Proofs using monad laws

To prove: 
   
  pure f <*> mx <*> my
=
  m >>= (\x -> my >>= \y -> return (f x y))

--------------------------------------- 

  pure f <*> mx <*> my
=                          -- add parentheses
  (pure f <*> mx) <*> my
=                          -- pure=return
  (return f <*> mx) <*> my
=                          -- rewrite inner <*>
  (return f >>= (\g->m2 >>= (\x -> return (g x)))) <*> my
=                          -- monad identity law
  ((\g->m2 >>= (\x -> return (g x))) f) <*> my
=                          -- lambda evaluation (it is more natural to combine this with the previous step)
  (m2 >>= (\x -> return (f x))) <*> my
=                          -- rewrite outer <*>, using a different name for the bound variable "x"
  (m2 >>= (\x -> return (f x))) >>= (\g -> my >>= (\y -> return (g y)))
=                          -- monad associativity law
  m2 >>= (\x -> return (f x) >>= (\g -> my >>= (\y -> return (g y))))
=                          -- monad identity law
  m2 >>= (\x -> my >>= (\y -> return (f x y)))
