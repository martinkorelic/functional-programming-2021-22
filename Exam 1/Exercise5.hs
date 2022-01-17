module Exercise5 where
import Control.Applicative (Alternative (empty))
import GHC.Base (Alternative((<|>)))

data Base = A | C | G | T

data Tree = Leaf Base | Fork Tree Tree

data Bit = O | I
    deriving Eq

newtype Inflater a = IF { inflate :: [Bit] -> Maybe (a, [Bit])}

compressBase :: Base -> [Bit]
compressBase A = [O,O]
compressBase C = [O, I]
compressBase G = [I, O]
compressBase T = [I, I]

compressTree :: Tree -> [Bit]
compressTree (Leaf a) = O : compressBase a
compressTree (Fork t1 t2) = I : compressTree t1 ++ compressTree t2

instance Functor Inflater where
    fmap f a = IF $ \bs -> case inflate a bs of
                            Nothing -> Nothing
                            Just (g, bits) -> Just (f g, bits)

instance Applicative Inflater where
    pure v = IF $ \bs -> Just (v, bs)
    abp <*> ap = IF $ \bs -> case inflate abp bs of
                                    Nothing -> Nothing
                                    Just (g, out) -> inflate (fmap g ap) out

instance Alternative Inflater where
    empty = IF $ \bs -> Nothing
    (<|>) p q = IF $ \bs -> case inflate p bs of
                                Nothing -> inflate q bs
                                just -> just

bit :: Bit -> Inflater ()
bit b = IF $ \bs -> case bs of 
                        [] -> Nothing
                        x:xs -> if x == b then Just ((), xs) else Nothing

base = IF $ \bs -> case bs of
                        O:O:xs -> Just (A, xs)
                        O:I:xs -> Just (C, xs)
                        I:O:xs -> Just (G, xs)
                        I:I:xs -> Just (T, xs)
                        rest -> Nothing

inflateTree :: Inflater Tree
inflateTree = (bit O *> pure Leaf <*> base) <|> (bit I *> pure Fork <*> inflateTree <*> inflateTree)