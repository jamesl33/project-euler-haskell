main = print $ sum $ filter even $ takeWhile (< 4000000) fibonacci
  where
    fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)
