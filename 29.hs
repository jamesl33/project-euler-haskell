import Data.List

-- terms :: Integer -> Integer -> [Integer]
-- terms l h = foldl' (\a c -> (terms' c) ++ a) [] [l .. h]
--   where
--     terms' n = foldl' (\a c -> a ++ [(n ^ c)]) [] [l .. h]

terms :: Integer -> Integer -> [Integer]
terms l h = concat [[i ^ j] | i <- [l .. h], j <- [l .. h]]

main = print $ length $ nub $ sort $ terms 2 100
