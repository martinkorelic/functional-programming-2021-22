import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as Map

word :: String
word = "the eastern spot is the nearest stop in earnest"

wordFrequency :: String -> [(String,Int)]
wordFrequency  = map (\x->(head x,length x)) . group . sort . words

--wordFrequency' :: String -> Map.Map k a
--wordFrequency' = 

mostFrequentOfLength :: Int -> String -> [(String, Int)]
mostFrequentOfLength n = reverse . sortOn snd . filter (\(w,l) -> length w >= n) . wordFrequency

wordLengthFrequency :: String -> [(Int, Int)]
wordLengthFrequency = sort . map (\x->(length (head x),length x)) . groupBy (\a b->length a == length b) . sortBy (\a b -> compare (length a)  (length b)) . words

anagrams :: String -> [[String]]
anagrams =  map (map snd) . filter (\l -> length l > 1) . groupBy (\a b -> fst a == fst b) . sort . map (\x -> (sort x, x)) . map head . group . sort . words

main :: IO ()
main = onStdin $ wordFrequency  -- change this to run a different function
  where onStdin f = getContents >>= mapM_ print . f . filter (\x->isAlphaNum x || isSpace x)
