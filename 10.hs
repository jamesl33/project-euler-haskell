import Data.List.Ordered (minus, union, unionAll)

main = print $ sum $ takeWhile (< 2000000) primes
  where
    primes = 2 : 3 : minus [5, 7 ..] (unionAll [[p * p, p * p + 2 * p ..] | p <- tail primes])
