import Control.Monad
import Data.Array
import Data.Array.MArray as MArray
import Data.Array.ST
import Data.List

divisors :: Integer -> [Integer]
divisors n = sort $ (1 :) $ nub $ concat [[x, div n x] | x <- [2 .. limit], n `rem` x == 0]
  where
    limit = (floor $ sqrt $ fromIntegral n)

abundant :: Integer -> Bool
abundant n = sum (divisors n) > n

expressible :: Integer -> Array Integer Bool
expressible n = runSTArray $ do
  arr <- MArray.newArray (0, n - 1) False
  forM_ [i + j | i <- abundantNumbers, j <- abundantNumbers, i + j < n] $ \i -> MArray.writeArray arr i True
  return arr
  where
    abundantNumbers = [n | n <- [1 .. n], abundant n]

main = print $ foldl' (\a e -> if not (snd e) then a + (fst e) else a) 0 (assocs (expressible 28123))
