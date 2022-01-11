module MyList where

data MyList a = a :# MyList a | Null
  deriving (Eq,Ord)

fromList :: [a] -> MyList a
fromList [] = Null
fromList (x:xs) = x :# fromList xs

toList :: MyList a -> [a]
toList Null = []
toList (a :# b) = a:toList b

{- fromList [1] <= fromList [1,2] -}

instance (Show a) => Show (MyList a) where
  show x = "fromList " ++ show (toList x)