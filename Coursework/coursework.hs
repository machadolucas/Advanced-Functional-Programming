import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map

-- | This is the main function to read an array of strings/lines and return the.
-- | ["axcxyd","axxcdyd"] -> 2 -> 4 -> 2 -> ["acd","acyd","axcd","axcy","axx","xcyd"]
funn :: [String] -> Int -> Int -> [(String,Int)]
funn [] _ _ = []
funn s g k = getPairsFromFile s g


-- | Get a list of tuples with the frequency of each pair with gap g for a file (a list of lines/strings)
-- |
-- | ["Namana","Ananas"] -> 2 -> [("aa",6),("am",1),("an",5),("as",2),("ma",2),("mn",1),("na",7),("nm",1),("nn",1),("ns",1)]
getPairsFromFile :: [String] -> Int -> [(String,Int)]
getPairsFromFile lines g = Map.toList
                             $ Map.fromListWith (\x y -> x + y) -- Sums the occurrences
                             $ map (\x ->(x,1)) -- Map to one occurrence per pair
                             $ concat -- Join pairs from lines
                             $ map (getPairsWithGapFromStr g) lines -- Get pairs from lines


-- | With maximum gap g between chars, generates all pairs of chars from lowercased
-- |    string str, considering only chars in US ASCII table.
-- | ##### I decided to ignore pairs in which there is 'space' char #####
-- |
-- | 2 -> "LucasM" -> ["lu","lc","la","uc","ua","us","ca","cs","cm","as","am","sm"]
getPairsWithGapFromStr :: Int -> String -> [String]
getPairsWithGapFromStr g str = [x:y:[] | (x:ys) <- tails (map toLower str), y <- ys,
                                        isAscii x && isAscii y &&
                                        not (isSpace x) && not (isSpace y) &&
                                        (fromJust $ elemIndex y ys) <= g]
