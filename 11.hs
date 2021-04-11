gridProduct :: [[Int]] -> Int -> Int -> Int -> Int -> Int
gridProduct grid startX startY dirX dirY =
  product [grid !! (startY + i * dirY) !! (startX + i * dirX) | i <- [0 .. 3]]

maxGridProduct :: [[Int]] -> Int
maxGridProduct grid = maximum $ concat [veru, verd, horl, horr, didr, diul, didl, diur]
  where
    veru = [gridProduct grid x y (-1) 0 | y <- [0 .. 19], x <- [4 .. 19]]
    verd = [gridProduct grid x y 1 0 | y <- [0 .. 19], x <- [0 .. 16]]
    horl = [gridProduct grid x y 0 (-1) | y <- [4 .. 19], x <- [0 .. 19]]
    horr = [gridProduct grid x y 0 1 | y <- [0 .. 16], x <- [0 .. 19]]
    didr = [gridProduct grid x y 1 1 | y <- [0 .. 16], x <- [0 .. 16]]
    diul = [gridProduct grid x y (-1) (-1) | y <- [4 .. 19], x <- [4 .. 19]]
    didl = [gridProduct grid x y 1 (-1) | y <- [4 .. 19], x <- [0 .. 16]]
    diur = [gridProduct grid x y (-1) 1 | y <- [0 .. 16], x <- [4 .. 19]]

main = readFile "p011_grid.txt" >>= print . maxGridProduct . (map (map read . words) . lines)
