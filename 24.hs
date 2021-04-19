import Data.List

main = putStrLn $ concat $ fmap show ((sort $ permutations [0 .. 9]) !! 999999)
