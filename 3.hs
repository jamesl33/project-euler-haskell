factorize :: Integer -> [Integer]
factorize n = factorize' n 2
  where
    factorize' 1 _ = []
    factorize' n f
      | n `mod` f == 0 = f : factorize' (n `div` f) f
      | otherwise = factorize' n (f + 1)

main = print $ last $ factorize 600851475143
