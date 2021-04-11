import Data.List (group, scanl1)

factorize :: Integer -> [Integer]
factorize n = factorize' n 2
  where
    factorize' 1 _ = []
    factorize' n f
      | n `mod` f == 0 = f : factorize' (n `div` f) f
      | otherwise = factorize' n (f + 1)

main = print $ head $ filter ((> 500) . divisors) triangles
  where
    divisors n = product $ map ((+ 1) . length) (group $ factorize n)
    triangles = scanl1 (+) [1 ..]
