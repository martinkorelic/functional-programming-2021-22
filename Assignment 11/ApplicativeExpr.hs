module ApplicativeExpr where

-- Applying "dr." to Maybe String
expr1 = ("dr." ++) <$> Just "Sjaak"

-- Filter if they're more than 1 and convert the whole list to Maybe
expr2 = pure (filter (\x->x>1)) <*> Just [1,2,3]

-- Filter if they're more than 1
expr3 = filter (>1) <$> Just [1,2,3]

-- Apply first 7 to mod and then 5
expr4 = mod <$> Just 7 <*> Just 5

-- Keeps applying a list of length of number of chars to produce
-- Eats up first argument of list and then second one of the second list
expr5 = replicate <$> [1,2,3] <*> ['a','b']