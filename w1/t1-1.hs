-- Write a function that, given three integers day, month and year, calculates if they represent a correct date.
-- Use nested if statements. Write also the type declaration of your function.

-- Dates in format yyyy mm dd
correctDate :: Int -> Int -> Int -> Bool
correctDate y m d = if y`mod`4 == 0 && m == 2 && d == 29
                        then True
                        else if m < 1 || m > 12
                                 then False
                                 else if d > 0 && d <= [31,28,31,30,31,30,31,31,30,31,30,31] !! (m-1)
                                        then True
                                        else False
