module Char where

import Data.Char

(~~) :: String -> String -> Bool
(~~) = \a b -> map toLower a == map toLower b

changeCase :: Char -> Char 
changeCase a = if isLower a then toUpper a else toLower a

reverseCase :: String -> String
reverseCase a = map changeCase a

shift :: Int -> Char -> Char
shift a '[' = shift a 'A'
shift 0 b = b
shift a b = if isLower b || isSpace b then b else shift (a-1) (succ b)

caesar :: Int -> String -> String
caesar a b = map (shift a . toUpper) b

msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"

-- using "caesar 5 msg" gives:
-- "FIRST I MUST SPRINKLE YOU WITH FAIRY DUST"