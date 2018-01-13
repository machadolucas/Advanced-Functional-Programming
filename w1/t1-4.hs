nextDate :: Int -> Int -> Int -> (Int, Int, Int)
nextDate d m y
    | m < 1 || m > 12 = error "Invalid month"
    | y`mod`4 == 0 && m == 2 && d == 29 = (1, 3, y) -- Final of February, leap year
    | y`mod`4 /= 0 && m == 2 && d == 28 = (1, 3, y) -- Final of February, non leap year
    | m == 12 && d == 31 = (1, 1, (y + 1)) -- Final of December, changes year
    | m `elem` [1,3,5,7,8,10] && d == 31 = (1, (m + 1), y) -- Final of 31 days months
    | m `elem` [4,6,9,11] && d == 30 = (1, (m + 1), y) -- Final of 30 days months
    | d < 1 || (y`mod`4 == 0 && d >= [31,29,31,30,31,30,31,31,30,31,30,31] !! (m-1)) = error "Invalid day"
    | y`mod`4 /= 0 && d >= [31,28,31,30,31,30,31,31,30,31,30,31] !! (m-1) = error "Invalid day"
    | otherwise = ((d + 1), m, y) -- Just sum one day

