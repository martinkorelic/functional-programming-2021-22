module MaybeMonad where
import Control.Monad (join)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap = fmap

stripMaybe :: Maybe (Maybe a) -> Maybe a
stripMaybe = join

applyMaybe:: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybe f a = a >>= f

monadMap :: (Monad m) => (a -> b) -> m a -> m b
monadMap = fmap

stripMonad :: (Monad m) => m (m a) -> m a
stripMonad = join

applyMonad:: (Monad m) => (a -> m b) -> m a -> m b
applyMonad f a = a >>= f