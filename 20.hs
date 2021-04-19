import Data.Char
import Data.List

main = print $ foldl' (\a d -> a + digitToInt d) 0 (show $ product [1 .. 100])
