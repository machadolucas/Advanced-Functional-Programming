-- Dates in format (yyyy,mm,dd)
getDay :: (Int, Int, Int) -> Int
getDay (_,_,d) = d
getMonth :: (Int, Int, Int) -> Int
getMonth (_,m,_) = m
getYear :: (Int, Int, Int) -> Int
getYear (y,_,_) = y

nextDate :: (Int, Int, Int) -> (Int, Int, Int)
nextDate date
    | (getMonth date) < 1 || (getMonth date) > 12 = error "Invalid month"
    | (getYear date)`mod`4 == 0 && (getMonth date) == 2 && (getDay date) == 29 = ((getYear date), 3, 1) -- Final of February, leap year
    | (getYear date)`mod`4 /= 0 && (getMonth date) == 2 && (getDay date) == 28 = ((getYear date), 3, 1) -- Final of February, non leap year
    | (getMonth date) == 12 && (getDay date) == 31 = ((getYear date) + 1, 1, 1) -- Final of December, changes year
    | (getMonth date) `elem` [1,3,5,7,8,10] && (getDay date) == 31 = ((getYear date), ((getMonth date) + 1), 1) -- Final of 31 days months
    | (getMonth date) `elem` [4,6,9,11] && (getDay date) == 30 = ((getYear date), ((getMonth date) + 1), 1) -- Final of 30 days months
    | (getDay date) < 1 || ((getYear date)`mod`4 == 0 && (getDay date) >= [31,29,31,30,31,30,31,31,30,31,30,31] !! ((getMonth date) -1)) = error "Invalid day"
    | (getYear date)`mod`4 /= 0 && (getDay date) >= [31,28,31,30,31,30,31,31,30,31,30,31] !! ((getMonth date) -1) = error "Invalid day"
    | otherwise = (((getYear date), (getMonth date), (getDay date) + 1)) -- Just sum one day

