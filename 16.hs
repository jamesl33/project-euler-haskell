import Data.Char (digitToInt)
import Data.List (foldl')

powerDigitSum :: Integer -> Integer -> Integer
powerDigitSum n p = foldl' (\a x -> a + toInteger (digitToInt x)) 0 (show $ n ^ p)

main = print $ powerDigitSum 2 1000
