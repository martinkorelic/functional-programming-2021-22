{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ListParse where

import Control.Applicative
import Control.Monad
import Parser

{- grammar:
 -   intList   = "{" { integer } "}"
 -}

intList :: Parser [Integer]
intList = (:) <$> symbol "{" *> many (space *> integer) <* symbol "}"

{- grammar:
 -   intRecord = "{" integer "#" { integer } "}"
 -                   ^ =: n      ^^^^^^^^^^^ (repeat n# times)
 -}

intRecord :: Parser [Integer]
intRecord = do
    symbol "{"
    n <- integer
    symbol "#"
    ns <- replicateM (fromInteger n) (do {space; integer})
    symbol "}"
    return ns