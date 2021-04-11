import Data.List (maximumBy)
import Data.Ord (comparing)

collatzN :: Int -> Int
collatzN n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzSeq :: Int -> [Int]
collatzSeq 1 = [1]
collatzSeq n = n : (collatzSeq $ collatzN n)

collatzLen :: Int -> Int
collatzLen n = collatzLen' 0 n
  where
    collatzLen' a 1 = a + 1
    collatzLen' a n = collatzLen' (a + 1) (collatzN n)

main = print $ fst $ maximumBy (comparing snd) [(n, collatzLen n) | n <- [1 .. 1000000]]
