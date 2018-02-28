-- The book used list comprehension to answer the following: Which right triangle that has integers for all sides
-- and all sides equal to or smaller than 10 has a perimeter of 24?
-- Do the same using the fact that list is an applicative functor, using the style of the applicatives
-- as proposed in the book.

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
