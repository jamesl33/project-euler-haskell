main = print $ length (takeWhile (\e -> length (show e) < 1000) fibonacci) + 1
  where
    fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)
