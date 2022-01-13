module FindDefs where
import Data.Maybe

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f = map (fromJust . f)

lift      :: (a -> b -> Maybe c) -> (Maybe a -> Maybe b -> Maybe c)
lift x a b = x (fromJust a) (fromJust b)

compute   :: (Monoid n) => (a -> n) -> [a] -> n
compute f = f . head

fuse      :: (a -> b -> c) -> (a -> b) -> a -> c
fuse f1 f2 a = f1 a (f2 a)