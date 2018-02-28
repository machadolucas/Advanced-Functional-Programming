-- Learn you a Haskell has example programs to maintain a todo list stored in a file. Make such a version that the
-- operation and its parameters are given as command line arguments, e.g.
-- todo add Clean up
-- todo del 2
--You may design the command syntax as you wish. You may assume always the same filename.

import System.Environment
import System.Directory
import System.IO
import Data.List

-- | Available usage:
--   add <text> :: Adds an item to list. E.g. "add I am a new item in the list!"
--   list :: Prints the list. E.g. "list"
--   del <[items]> :: Delete one or more items in the list specified by the indexes. E.g. "del 1 3 4"
--   *Data is saved in todo.txt file*
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action $ args

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("list", list)
            , ("del", del)
            ]

add :: [String] -> IO ()
add todoItems = appendFile "todo.txt" (intercalate " " todoItems ++ "\n")

list :: [String] -> IO ()
list _ = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

del :: [String] -> IO ()
del numberStrings = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let numbers = readNumbers numberStrings
        todoTasks = lines contents
        newTodoItems = foldr1 intersect $ map ($ todoTasks) $ map delete (map (todoTasks !!) numbers)
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"

readNumbers :: [String] -> [Int]
readNumbers = map read

deleteAll :: [String] -> [String] -> [String]
deleteAll _ [] = []
deleteAll [] list = list
deleteAll (x:xs) list = delete x list
