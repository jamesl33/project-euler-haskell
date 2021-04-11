factorial :: Integer -> Integer
factorial n = product [1 .. n]

combinations :: Integer -> Integer -> Integer
combinations n k = (factorial n) `div` ((factorial k) * (factorial (n - k)))

main = print $ combinations 40 20
