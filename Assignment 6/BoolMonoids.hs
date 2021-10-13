module Monoids where
import Data.Monoid

-- 1
newtype And = And {getAnd :: Bool}
    deriving (Show)

instance Semigroup And where
    (<>) a b = And (getAnd a && getAnd b)

instance Monoid And where
    mempty = And True

--"And True <> And True" will return: "And {getBool = True}"
-- a && (b && c) = (a && b) && c
-- True && a = a

newtype Or n = Or {getOr::Bool}
    deriving (Show)

instance Semigroup (Or n) where
    (<>) (Or a) (Or b) = Or (a || b)

instance Monoid (Or n) where
    mempty = Or False

-- a || (b || c) = (a || b) || c
-- False || a = a

newtype Xor n = Xor {getXor :: Bool}
    deriving (Show)

instance Semigroup (Xor n) where
    (<>) (Xor a) (Xor b) = Xor (a == b)

instance Monoid (Xor n) where
    mempty = Xor False

-- a xor (b xor c) = (a xor b) xor c
-- False (xor) a = a

newtype XNor n = XNor {getXNor::Bool}
    deriving (Show)

instance Semigroup (XNor n) where
    (<>) (XNor a) (XNor b) = XNor (a /= b)

instance Monoid (XNor n) where
    mempty = XNor True

-- a xnor (b xnor c) = (a xnor b) xnor c
-- True (xnor) a = a


-- 2

-- Concating the whole And list and producing a single value that is either false or true.

-- And: False, if any of the booleans are false, else if every boolean is true, the outcome is true

-- Or: True, if atleast one of the items is true, else if every item is false it will be false

-- Xor: If there are odd number of true items it will be true, if there are even number of true items it will be false.

-- XNor: If there are even number of true items it will be true, if there are odd numbers of true items it will be false.