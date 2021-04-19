import Data.Char
import Data.List
import Data.List.Split

score :: String -> Int
score xs = foldl' (\a b -> a + (ord $ toLower b) - (ord 'a') + 1) 0 xs

total :: [String] -> Int
total xs = snd $ foldl' (\a b -> ((fst a) + 1, (snd a) + (fst a) * score b)) (1, 0) xs

main = readFile "p022_names.txt" >>= print . total . parse
  where
    parse = sort . (map init) . (map (drop 1)) . splitOn ","
