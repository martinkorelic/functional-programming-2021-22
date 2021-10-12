module Monoids where
import Data.Monoid

-- 1

newtype Boolean x y = Boolean { fromBool :: Bool }
    deriving (Show)

instance Semigroup Boolean where
    a <> b  = Boolean (a <> b)

--instance Monoid And where
--    mempty = True
--    mappend = Boolean (fromBool x && fromBool y)

-- Depends on P
-- p && t = p
-- p || t = t || p = p
-- xor
-- xnor