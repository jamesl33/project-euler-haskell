import Data.List

isLeapYear :: Int -> Bool
isLeapYear x = x `rem` 4 == 0 && not ((x `rem` 100 == 0) && (x `rem` 400 /= 0))

sundaysOnFirst :: Int -> Int -> Int
sundaysOnFirst t f = snd (foldl' year (0, 0) [t .. f])
  where
    year a y = foldl' (\a m -> (fst a + month y m, inc a)) a [0 .. 11]
    month y m
      | m == 1 = if isLeapYear y then 29 else 28
      | otherwise = [31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! m
    inc a
      | fst a `rem` 7 == 0 = snd a + 1
      | otherwise = snd a

main = print $ sundaysOnFirst 1901 2000
