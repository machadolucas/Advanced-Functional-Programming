-- Make a cipher that takes a string to be ciphered and parameters k and n, and works as follows: Take k first
--  characters, and reverse their order. Then take k next characters, and reverse their order, etc. If finally
--  there is a string with less than k characters, just reverse their order. This way, if the string is k and
--  n is 3, then "this way" 3 results into "ihtw sya". Then apply the Ceasar cipher (from the book) to the
--  result with parameter n.
-- Write also a function to decipher.

import Data.Char

-- Encodes with Caesar cipher
encodeCaesar :: Int -> String -> String
encodeCaesar shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

-- Decodes with Caesar cipher
decodeCaesar :: Int -> String -> String
decodeCaesar shift msg = encodeCaesar (negate shift) msg

-- Reverse the String for each group of k chars
reversek :: Int -> String -> String
reversek k txt = if length txt < 1 then []
                 else reverse (take k txt) ++ reversek k (drop k txt)

-- Final cipher function
cipher :: String -> Int -> Int -> String
cipher txt k n = encodeCaesar n (reversek k txt)

-- Final decipher function
decipher :: String -> Int -> Int -> String
decipher txt k n = decodeCaesar n (reversek k txt)
