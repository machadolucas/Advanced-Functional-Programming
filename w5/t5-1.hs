-- The book introduces the safe calculator in Chapter "For a few monads more", using the reads function.
-- Make a version of the safe calculator that runs as a compiled program and reads input from standard
-- input (like terminal).

import System.Environment
import System.IO
import Control.Monad
import Data.List

-- | Usage: runhaskell t5-1.hs <args>
-- | Example: runhaskell t5-1.hs 40 2 +
main = do
    args <- getArgs
    putStrLn $ show $ solveRPN $ intercalate " " args

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "+" = return ((x + y):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

