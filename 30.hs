import Data.Char
import Data.List

sumFifthPowers :: Int -> Int
sumFifthPowers n = foldl' (\a c -> a + (digitToInt c) ^ 5) 0 (show n)

main = print $ sum [n | n <- [64 .. 295244], sumFifthPowers n == n]
