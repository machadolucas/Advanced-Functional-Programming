-- We say that character pair (c1,c2) appear in string s with gap constraint  g, if c1 is before c2 and there are
-- at most g characters between c1 and c2 in s. Write a function that, given a gap g and strings s, finds all
-- character pairs that appear with gap g in s.

-- | Gets all possible pairs between c1 and st. Example for 'a' and "bcd": [('a','b'),('a','c'),('a','d')]
allPairs :: Char -> String -> [(Char,Char)]
allPairs c1 st = [(c1, c2) | c2 <- st ]

-- | Main function that gets all pairs with at most gap g in provided string
pairs :: Int -> String -> [(Char,Char)]
pairs g [] = []
pairs g (s:[]) = []
pairs g (c1:c2:[]) = [(c1,c2)]
pairs g s@(c1:x)
    | length s >= g + 2 = (allPairs c1 (take (g + 1) x)) ++ pairs g x
    | otherwise =  pairs (length s - 2) s
