-- Now solve the triangle problem of 5.2 using lists as monads (do notation) and the guard function
-- introduced in the Chapter "A handful of Monads".

import Control.Monad

main = do
    putStrLn $ show rightTriangles

rightTriangles :: [(Int,Int,Int)]
rightTriangles = do
    c <- [1..10]
    b <- [1..c]
    a <- [1..b]
    guard (a^2 + b^2 == c^2)
    guard (a+b+c == 24)
    return (a,b,c)
