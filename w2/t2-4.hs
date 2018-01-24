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

