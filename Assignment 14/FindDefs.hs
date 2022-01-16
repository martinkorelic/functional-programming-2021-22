module FindDefs where

bindSt  :: (a -> s -> (b,s)) -> (s -> (a,s)) -> (s -> (b,s))
bindSt f1 f2 s = f1 s1 s2
    where
        (s1,s2) = f2 s 

andThen  :: ((a -> r) -> r) -> ((b -> r) -> a -> r) -> (b -> r) -> r
andThen f1 f2 f3 = f1 (f2 f3)