module RandomState where

import System.Random

{- Note: the definitions below are more easy to understand versions of the
 - official MonadState monad, which is in Control.Monad.State. The difference
 - is that our version hardcodes the type of the state as a global parameter
 - whereas in the official version, it is given as a parameter.
 -
 - In terms of the official version, everything specified below is equivalent to:
 -
 - import Control.Monad.State
 - type RandomState a = State StdGen a
 -}

type GlobalState = StdGen

newtype RandomState a = St { runState :: GlobalState -> (a, GlobalState) }

--evaluate inside the state monad, but throw away the state afterwards
evalState :: RandomState a -> GlobalState -> a
evalState st g = fst (runState st g)

--evaluate inside the state monad purely for the side effect on the state
execState :: RandomState a -> GlobalState -> GlobalState
execState st g = snd (runState st g)

--place a "state transformation" function directly
state :: (GlobalState -> (a, GlobalState)) -> RandomState a
state f = St f

instance Functor RandomState where
  fmap f sx = St $ \s1 -> let (x,s2) = runState sx s1
                          in  (f x, s2)

-- boilerplate instance of Applicative
instance Applicative RandomState where
  pure      = return
  m1 <*> m2 = do { f <- m1; x <- m2; return (f x) }

instance Monad RandomState where
  return x = St $ \s -> (x, s)
  sx >>= k = St $ \s1 -> let (x, s2) = runState sx s1
                             (y, s3) = runState (k x) s2
                         in  (y, s3)

-- note: we use "get" and "put" here for compatibility with the "official" MonadState
class (Monad m) => MonadState m where
  get :: m GlobalState
  put :: GlobalState -> m ()

instance MonadState RandomState where
  get   = St $ \s -> (s,s)
  put s = St $ \_ -> ((), s)
