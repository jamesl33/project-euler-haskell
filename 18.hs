import Data.List (foldr1)

sumPair, maxPair :: (Int, Int) -> Int
sumPair (a, b) = a + b
maxPair (a, b) = if a > b then a else b

pairsInRow :: [Int] -> [(Int, Int)]
pairsInRow [] = []
pairsInRow xs | length xs < 2 = []
pairsInRow (x : y : xs) = [(x, y)] ++ pairsInRow ([y] ++ xs)

maxPairsInRow :: [Int] -> [Int]
maxPairsInRow [x] = [x]
maxPairsInRow xs = fmap maxPair (pairsInRow xs)

maxPathSum :: [[Int]] -> Int
maxPathSum = head . foldr1 (\a b -> fmap sumPair (zip a (maxPairsInRow b)))

main = readFile "p018_triangle.txt" >>= print . maxPathSum . (map (map read . words) . lines)
