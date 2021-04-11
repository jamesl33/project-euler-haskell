import Data.Char (digitToInt)
import Data.List (foldl')

main = readFile "p013_numbers.txt" >>= print . (parseInteger . take 10 . show . sum) . (map parseInteger . lines)
  where
    parseInteger xs = foldl' (\a x -> a * 10 + toInteger (digitToInt x)) 0 xs
