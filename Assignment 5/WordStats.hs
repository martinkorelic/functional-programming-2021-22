import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M

wordFrequency :: String -> [(String,Int)]
wordFrequency  = map (\x->(head x,length x)) . group . sort . words

--mostFrequentOfLength :: Int -> String -> ??

--wordLengthFrequency :: String -> ??

--anagrams :: String -> ??

main :: IO ()
main = onStdin $ wordFrequency  -- change this to run a different function
  where onStdin f = getContents >>= mapM_ print . f . filter (\x->isAlphaNum x || isSpace x)
