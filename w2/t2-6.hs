-- Write a function that take as parameters:
-- - a function f that, given two strings, calculates a similarity score for them (a simple one would be e.g. the
--   number of characters appearing in both), and
-- - two strings
-- calculates the similary of the strings.
-- Your function's type should be: (String-> String -> Int) -> String -> String ->Int
-- After that, use partial evaluation to create another function, where a function and a comparision string has been
--   fixed. Do that (a) by fixing f with a lambda function and (b) using a named function.

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
