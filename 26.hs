import Data.List
import Data.Ord

cycleLength d = remainders d 10 []
  where
    remainders d r rs
      | d == 0 = 0
      | otherwise =
        let r' = r `mod` d
         in case elemIndex r' rs of
              Just i -> i + 1
              Nothing -> remainders d (10 * r') (r' : rs)

main = print $ fst $ maximumBy (comparing snd) [(d, cycleLength d) | d <- [2 .. 1000]]
