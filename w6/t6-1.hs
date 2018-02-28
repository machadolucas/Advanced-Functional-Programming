-- Learn you a Haskell has an example for deleting an entry from the to-do file. The example uses a temporary file,
-- which avoids conflicts due to lazy IO. Change the example so that no temporary files are used and use Text instead
-- of String to enable evaluation-at-once.

import System.IO
import Data.List
import qualified Data.Text as Tx
import qualified Data.Text.IO as TxIO

main = do
    file <- TxIO.readFile "todo.txt"
    let contents = Tx.unpack file
        todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    TxIO.writeFile "todo.txt" $ Tx.pack (unlines newTodoItems)
