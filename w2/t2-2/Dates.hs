module Dates
( correctDate
, funn
) where

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

-- |Use example: correctDate (20,2,1992), returns True
correctDate :: (Int, Int, Int) -> Bool
correctDate date
    | (getYear date)`mod`4 == 0 && (getMonth date) == 2 && (getDay date) == 29 = True
    | (getMonth date) > 0 && (getMonth date) < 13 && (getDay date) > 0 && (getDay date) <= [31,28,31,30,31,30,31,31,30,31,30,31] !! ((getMonth date)-1) = True
    | otherwise = False

-- |Use example: funn (20,2,1992) (15,3,1992) 10, returns 10 dates between the intervals
funn :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> [(Int, Int, Int)]
funn dateStart dateEnd n = take n [(d,m,y) | y <- [0..2100], m <- [1..12], d <- [1..31], correctDate (d,m,y), comparableDate dateStart < (y,m,d), comparableDate dateEnd > (y,m,d) ]
