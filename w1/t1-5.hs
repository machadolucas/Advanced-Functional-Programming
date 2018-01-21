-- Dates in format (yyyy,mm,dd)
-- Use example: funn (1992,2,20) (1992,3,15)
getDay :: (Int, Int, Int) -> Int
getDay (_,_,d) = d
getMonth :: (Int, Int, Int) -> Int
getMonth (_,m,_) = m
getYear :: (Int, Int, Int) -> Int
getYear (y,_,_) = y

correctDate :: (Int, Int, Int) -> Bool
correctDate date
    | (getYear date)`mod`4 == 0 && (getMonth date) == 2 && (getDay date) == 29 = True
    | (getMonth date) > 0 && (getMonth date) < 13 && (getDay date) > 0 && (getDay date) <= [31,28,31,30,31,30,31,31,30,31,30,31] !! ((getMonth date)-1) = True
    | otherwise = False

funn :: (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int)]
funn dateStart dateEnd = [(y,m,d) | y <- [0..2100], m <- [1..12], d <- [1..31], correctDate (y,m,d), dateStart < (y,m,d), dateEnd > (y,m,d) ]



