module Pow2 where

pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)


--2.3.3
--The maximum n for which 2^n can be represented for:
--Integer doesn't have a maximum
--Int has a maximum of n=62
--Float reaches infinity after n=127
--Double reaches infinity after n=1023