-- Make a module that contains at least two functions for dates from last week exercises.
-- Make also a test function that imports the module and uses the functions.

import Dates

-- | Tests function correctDate for leap years from 2018 to 2030
testFunction :: [(Int, Bool)]
testFunction = [(year, correctDate (29,2,year)) | year <- [2018..2030]]

-- | Tests function funn to get 10 dates from interval between (20,2,1992) and (15,3,1992)
testFunction2 :: [(Int, Int, Int)]
testFunction2 = funn (20,2,1992) (15,3,1992) 10
