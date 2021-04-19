import Data.List

divisors :: Integer -> [Integer]
divisors n = sort $ (1 :) $ nub $ concat [[x, div n x] | x <- [2 .. limit], n `rem` x == 0]
  where
    limit = (floor $ sqrt $ fromIntegral n)

sumDivisors :: Integer -> Integer
sumDivisors n = sum $ divisors n

amicable :: Integer -> Bool
amicable n = s /= n && sumDivisors s == n
  where
    s = sumDivisors n

main = print $ sum [x | x <- [1 .. 10000], amicable x]
