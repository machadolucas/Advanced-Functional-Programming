import System.Environment
import System.Directory
import System.IO
import System.Random
-- Implement a program that is given a filename and an integer k as command line arguments. The program should
-- output k randomly selected lines from the file.

-- | Usage: runhaskell t4-4.hs <filename> <k>
main = do
    (filename:k:xs) <- getArgs
    printRandomLines filename (read k)

printRandomLines :: String -> Int -> IO ()
printRandomLines filename k = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
    let amountLines = length fileLines - 1
    gen <- getStdGen
    let randomIndexes = take k $ randomRs (0, amountLines) gen :: [Int]
    let chosenLines = map (fileLines !!) randomIndexes
    mapM_ print chosenLines
    hClose handle
