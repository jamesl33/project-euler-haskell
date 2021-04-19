import Data.List
import Data.Ord

prime :: Integer -> Bool
prime n
  | n <= 3 = n > 1
  | n `rem` 2 == 0 || n `rem` 3 == 0 = False
  | otherwise = prime' 5
  where
    prime' i
      | i ^ 2 > n = True
      | n `rem` i == 0 || n `rem` (i + 2) == 0 = False
      | otherwise = prime' (i + 6)

primes :: Integer -> Integer -> Integer
primes a b = primes' a b 0
  where
    primes' a b n
      | not (prime (n ^ 2 + (a * n) + b)) = n
      | otherwise = primes' a b (n + 1)

main = print $ prod $ fst (maximumBy (comparing snd) [((a, b), primes a b) | a <- [(-1000) .. 999], b <- [-1000 .. 1000]])
  where
    prod a = (fst a) * (snd a)
