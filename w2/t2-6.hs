import Data.List

-- |Calculates a similarity score between two strings by counting common characters.
simCommon :: String -> String -> Int
simCommon s1 s2 = length (intersect s1 s2)

-- |Receives a function to calculate similarity and two strings, returning a score
similarity :: (String -> String -> Int) -> String -> String -> Int
similarity f s1 s2 = f s1 s2

-- | Partial evaluation fixing the function and a string, using lambda (a)
simpleSimilarity1 :: String -> Int
simpleSimilarity1 s1 = similarity (\s1 s2 -> length (intersect s1 s2)) s1 "haskelliscool"

-- | Partial evaluation fixing the function and a string, using a named function (b)
simpleSimilarity2 :: String -> Int
simpleSimilarity2 s1 = similarity simCommon s1 "haskelliscool"
