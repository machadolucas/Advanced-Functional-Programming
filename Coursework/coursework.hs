import System.IO
import qualified Data.Text as Tx
import qualified Data.Text.IO as TxIO
import qualified Data.Map as Map
import Data.List
import Data.Function (on)
import Data.Char
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (replicateM)

-- | Haskell runtime has an efficient thread manager, therefore I assumed we can create a thread for each file.

main = do
     -- Reads input.txt and parse g, k and fileNames
    file <- TxIO.readFile "input.txt"
    let inputContents = Tx.unpack file
        inputLines = lines inputContents
        g = read $ inputLines !! 0 :: Int
        k = read $ inputLines !! 1 :: Int
        filesNames = drop 2 inputLines

    -- Create channels and fork threads for each file
    --fileNamesChunks = chunksOf 4 $ drop 2 inputLines
    threadResults <- newChan
    mapM_ (forkIO . processFile threadResults g) filesNames
    -- Calculates results after threads are done
    results <- replicateM (length filesNames) $ readChan threadResults
    let groupedResults = unzip results
        totalLines = foldl (+) 0 $ fst groupedResults
        kMostFrequentPairs = take k
                            $ sortBySnd
                            $ Map.toList
                            $ Map.fromListWith (\x y -> x + y)
                            $ concat
                            $ snd groupedResults
        outputContent = map (\(x,y) -> generateOutputLine x y totalLines) kMostFrequentPairs
    -- Outputs results
    TxIO.writeFile "output.txt" $ Tx.pack (unlines outputContent)


-- | Creates an formatted output line form the results
-- |
-- | "a b 2300 45678"
generateOutputLine :: String -> Int -> Int -> String
generateOutputLine pair freq totalLines = (head pair) : " " ++
                                      (tail pair) ++ " " ++
                                       (show freq) ++ " " ++
                                       (show totalLines)


-- | Sorts a list of tuples by snd of the tuple.
-- |
-- | [("aa",5),("bb",8),("cc",6)] -> [("bb",8),("cc",6),("aa",5)]
sortBySnd :: [(a, Int)] -> [(a, Int)]
sortBySnd = sortBy (flip compare `on` snd)


-- | Processes a individual file getting the pairs map and amount of lines and writing to the channel
processFile :: Chan (Int, [(String, Int)]) -> Int -> String -> IO ()
processFile threadResults g fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
        linesNumber = length fileLines
        resultsMap = getPairsFromFile fileLines g
    writeChan threadResults (linesNumber, resultsMap)


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
