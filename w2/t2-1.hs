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
