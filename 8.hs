import Data.Char (digitToInt)

maxAdjacent :: String -> Int
maxAdjacent xs = maximum $ fmap prod [s | i <- [0 .. 999], let s = slice xs i 13, length s == 13]
  where
    prod xs = product $ map digitToInt xs
    slice s o l = take l (drop o s)

main = readFile "p008_number.txt" >>= print . maxAdjacent . (concat . lines)
