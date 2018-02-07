import Data.List

-- |Make a version of the Polish calculator that reads the input line by line and after each input it outputs the
-- stack (the list containing the stack). In addition to the operations in the book, implement a pop command that
-- just takes away the top element of the stack.

main = do
     line <- getLine
     putStr $ show (words line) -- I guess this is "output the stack" (?)
     putStr " = "
     putStrLn $ show (solveRPN line) -- I also output the result, otherwise what is the purpose?

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:ys) "pop" = ys -- Custom implemented 'pop' command
            foldingFunction xs numberString = read numberString:xs
