module ThereCanBeOnlyOne where

onlyElem :: (Eq a) => a -> [a] -> Bool
onlyElem a = (<2) . length . filter (==a)

onlyOnce :: (a -> Bool) -> [a] -> Bool
onlyOnce f = (<2) . length . filter f

onlyElemOne :: (Eq a) => a -> [a] -> Bool
onlyElemOne a = onlyOnce (==a)