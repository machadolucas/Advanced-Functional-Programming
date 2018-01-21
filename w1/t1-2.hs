-- Dates in format yyyy mm dd
correctDate :: Int -> Int -> Int -> Bool
correctDate y m d
    | y`mod`4 == 0 && m == 2 && d == 29 = True
    | m > 0 && m < 13 && d > 0 && d <= [31,28,31,30,31,30,31,31,30,31,30,31] !! (m-1) = True
    | otherwise = False
