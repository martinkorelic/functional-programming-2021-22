module ADTs where

data Day       = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Show

weekend :: [Day]
weekend = [Sat,Sun]

data Prop      = Prop :-> Prop | T | F
  deriving Show

evalProp :: Prop -> Prop
evalProp T = T
evalProp F = F
evalProp (x :-> y) = case (x,y) of
                      (T,F) -> F
                      _ -> evalProp x :-> evalProp y

data Unit      = Unit
  deriving Show

multiplx :: Int -> Unit -> [Unit]
multiplx x Unit = replicate x Unit

data Foo a     = Bar a
  deriving Show

tr :: Foo a -> a
tr (Bar a) = a

data Tuple a b = Two a b | One a | None
  deriving Show

mktup :: Num b => Tuple b b -> (b, b)
mktup None = (0,0)
mktup (One a) = (a,a)
mktup (Two a b) = (a,b)

data Wrapped a = Wrapped (Wrapped a) | Bare a
  deriving Show

unwrap :: Wrapped p -> p
unwrap (Bare a) = a
unwrap (Wrapped a) = unwrap a