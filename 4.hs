main = print $ maximum [p | x <- [100 .. 1000], y <- [100 .. 1000], let p = x * y, show p == reverse (show p)]
