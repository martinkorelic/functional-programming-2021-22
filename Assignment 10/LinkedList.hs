module LinkedList where

import Data.IORef

type ListRef elem = IORef (List elem)
data List elem = Nil | Cons elem (ListRef elem) 

nil :: IO (ListRef elem) 
nil = newIORef Nil 

cons :: elem -> ListRef elem -> IO (ListRef elem)
cons x ref = newIORef (Cons x ref) 

fromList :: [elem] -> IO (ListRef elem)
fromList []     = nil
fromList (x:хs) = fromList хs >>= cons x 

toList :: ListRef elem -> IO [elem]
toList ref = do 
  list <- readIORef ref
  case list of
    Nil           -> return []
    (Cons x ref') -> do { xs <- toList ref'; return (x:xs) }

foreach :: ListRef a -> (a -> IO b) -> IO (ListRef b) 
foreach ref action = toList ref >>= mapM action >>= fromList 

-- only works for actions of type "a -> IO a", since otherwise there is no way to alter the IORef in-place!
-- also, we don't really need to return anything anymore.
inplace_foreach :: ListRef a -> (a -> IO a) -> IO () 
inplace_foreach ref action = do 
  list <- readIORef ref
  case list of  
    Nil           -> return ()
    (Cons x ref') -> do { x' <- action x; writeIORef ref (Cons x' ref'); inplace_foreach ref' action } 
