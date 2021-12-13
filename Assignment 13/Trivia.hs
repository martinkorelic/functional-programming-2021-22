module Trivia where

import Control.Applicative
import Parser

dot :: Parser Char
dot = char '.'

{-

1. Parses first dot character into Just ('char', "rest of string")
if there is a char to parse, otherwise returns Nothing.

2. Parses many dot characters until it hits something that isn't a dot. 

-}
-- 3. This causes infinite loop because it is parsed many times
-- no matter if it fails or is correct
--loop :: Parser Char
--loop = many (many char)

dots :: Parser String
dots = (++) <$> many dot <*> many dot
-- 4. All dots until the it hits something other than a dot, which is parsed by the other many dot.