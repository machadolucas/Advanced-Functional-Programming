-- Old solution using list comprehensions:
-- let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

import Control.Applicative
import Data.List

main = do
    let rightTriangles' = filter isRightTriangle $ liftA3 (,,) [1..10] [1..10] [1..10]
    putStrLn $ show rightTriangles'


isRightTriangle :: (Int,Int,Int) -> Bool
isRightTriangle (a,b,c)
      | a^2 + b^2 == c^2 && a+b+c == 24 = True
      | otherwise = False
