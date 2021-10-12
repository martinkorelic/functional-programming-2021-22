module Monoids where
import Data.Monoid

newtype And = And{getBool :: Bool}
    deriving (Show)

instance Monoid And where
    mempty = And True

instance Semigroup And where
    (<>) a b = And (getBool a && getBool b)

--"And True <> And True" will return: "And {getBool = True}"


-- Depends on P
-- p && t = p
-- p || t = t || p = p
-- xor
-- xnor
