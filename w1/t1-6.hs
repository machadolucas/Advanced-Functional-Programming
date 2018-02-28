-- Write a function that, given a start date and an end date as (day,month,year) tuples and an integer n,
--  calculates a list of consecutive dates starting from the start date. If there are n such dates smaller
--  or equal than the end date, then n first dates are put into the list. If not, then all the dates from
--  the start date to the end date are put into the list.

-- Dates in format (d,m,y)
getDay :: (Int, Int, Int) -> Int
getDay (d,_,_) = d
getMonth :: (Int, Int, Int) -> Int
getMonth (_,m,_) = m
getYear :: (Int, Int, Int) -> Int
getYear (_,_,y) = y

-- Transforms (d,m,y) to (y,m,d), so date tuples can be compared naturally
comparableDate :: (Int, Int, Int) -> (Int, Int, Int)
comparableDate date = ((getYear date),(getMonth date),(getDay date))

correctDate :: (Int, Int, Int) -> Bool
correctDate date
    | (getYear date)`mod`4 == 0 && (getMonth date) == 2 && (getDay date) == 29 = True
    | (getMonth date) > 0 && (getMonth date) < 13 && (getDay date) > 0 && (getDay date) <= [31,28,31,30,31,30,31,31,30,31,30,31] !! ((getMonth date)-1) = True
    | otherwise = False

-- Use example: funn (20,2,1992) (15,3,1992) 10
funn :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> [(Int, Int, Int)]
funn dateStart dateEnd n = take n [(d,m,y) | y <- [0..2100], m <- [1..12], d <- [1..31], correctDate (d,m,y), comparableDate dateStart < (y,m,d), comparableDate dateEnd > (y,m,d) ]
