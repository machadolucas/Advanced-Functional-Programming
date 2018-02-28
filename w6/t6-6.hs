-- Write QuickCheck tests to test Task2.4.

-- Write a function that, given two strings s1 and s2, computes a string from s1 by deleting all the characters that
-- appear also in s2. If a character, say 'a', appears in s2 k times, then the function can only remove up to k
-- occurences of that character from s1. E.g. s1 = "aabbccdd", s2="bbbbad" -> "accd".

import Test.QuickCheck

-- | Removes an item from a list, only once (not all occurrences)
removeOnce :: (Eq a) => a -> [a] -> [a]
removeOnce _ [] = []
removeOnce x lst
  | x == y = ys
  | otherwise = y:removeOnce x ys
  where
    y:ys = lst

-- |Deletes all chars in s1 that appears in s2, limited to k occurrences of the char as they appear k times in s2.
del :: String -> String -> String
del [] [] = []
del s1 [] = s1
del s1 (s2:[]) = removeOnce s2 s1
del s1 (s:s2) = del (removeOnce s s1) s2


prop_example = del "aabbccdd" "bbbbad" == "accd"

prop_emptys1 s2 = del "" s2 == ""

prop_emptys2 s1 = del s1 "" == s1

prop_one = del "aabbccdd" "b" == "aabccdd"
