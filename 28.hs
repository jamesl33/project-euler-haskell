diags :: Integer -> Integer
diags n = sum [(se i) + (sw i) + (nw i) + (ne i) | i <- [1 .. n `div` 2]] + 1
  where
    se n = 4 * (n * n) - (2 * n) + 1
    sw n = 4 * (n * n) + 1
    nw n = 4 * (n * n) + (2 * n) + 1
    ne n = 4 * (n * n) + (4 * n) + 1

main = print $ diags 1001
