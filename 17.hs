numToWords n
  | n < 20 = ones !! n
  | n < 100 && n `rem` 10 == 0 = tens !! (n `div` 10)
  | n < 100 = (tens !! (n `div` 10)) ++ (ones !! (n `rem` 10))
  | n < 1000 && n `rem` 100 == 0 = (ones !! (n `div` 100)) ++ "hundred"
  | n < 1000 = (ones !! (n `div` 100) ++ "hundred" ++ "and" ++ numToWords (n `rem` 100))
  | n <= 10000 && n `rem` 1000 == 0 = (ones !! (n `div` 1000)) ++ "thousand"
  | n <= 10000 = (ones !! (n `div` 1000)) ++ "thousand" ++ numToWords (n `rem` 1000)
  where
    ones =
      [ "zero",
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine",
        "ten",
        "eleven",
        "twelve",
        "thirteen",
        "fourteen",
        "fifteen",
        "sixteen",
        "seventeen",
        "eighteen",
        "nineteen"
      ]
    tens =
      [ "",
        "",
        "twenty",
        "thirty",
        "forty",
        "fifty",
        "sixty",
        "seventy",
        "eighty",
        "ninety"
      ]

main = print $ length $ concat [numToWords n | n <- [1 .. 1000]]
