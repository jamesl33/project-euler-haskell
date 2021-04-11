import Data.List.Ordered (minus, union, unionAll)

main = print $ primes !! 10000
  where
    primes = 2 : 3 : minus [5, 7 ..] (unionAll [[p * p, p * p + 2 * p ..] | p <- tail primes])
