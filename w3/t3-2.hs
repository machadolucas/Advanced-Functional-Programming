data Date = Date { day :: Int
                 , month :: Int
                 , year :: Int }

instance Show Date where
    show (Date { day = d, month = m, year = y }) = (getMonthStr m) ++ " " ++ (getDayStr d) ++  ", " ++ (show y)

getDayStr :: Int -> String
getDayStr n
    | n < 1 || n > 31 = error "Invalid day"
    | n == 1 = "1st"
    | n == 2 = "2nd"
    | n == 3 = "3rd"
    | otherwise = show n ++ "th"

getMonthStr :: Int -> String
getMonthStr n
    | n == 1 = "January"
    | n == 2 = "February"
    | n == 3 = "March"
    | n == 4 = "April"
    | n == 5 = "May"
    | n == 6 = "June"
    | n == 7 = "July"
    | n == 8 = "August"
    | n == 9 = "September"
    | n == 10 = "October"
    | n == 11 = "November"
    | n == 12 = "December"
    | otherwise = error "Invalid month"

validDate :: Date -> Bool
validDate (Date { day = d, month = m, year = y })
    | y`mod`4 == 0 && m == 2 && d == 29 = True
    | m > 0 && m < 13 && d > 0 && d <= [31,28,31,30,31,30,31,31,30,31,30,31] !! (m-1) = True
    | otherwise = False
