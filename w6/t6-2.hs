-- Make a version of Ceaser's cipher that is given as command line arguments a filename and the encoding number.
-- Read the input file and overwrite it with the ciphered contents. Use Bytestrings to read and write the file.

import System.IO
import System.Environment
import qualified Data.ByteString as B

-- | Usage: runhaskell t6-2.hs <filename> <k>
main = do
    (filename:k:xs) <- getArgs
    contents <- B.readFile filename
    let encoded = encodeCaesar (read k) contents
    B.writeFile filename encoded


-- Encodes with Caesar cipher
encodeCaesar :: Int -> B.ByteString -> B.ByteString
encodeCaesar k msg =
    let ords = map fromEnum $ B.unpack msg
        shifted = map (+ k) ords
    in  B.pack $ map toEnum shifted

