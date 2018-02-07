--We say that the pair of elements (a,b) appears in a list s with gap constraing g, if a is before b in s and there
-- are at most g elements between a and b in s.
--A sequence of elements [a1,a2,...,ak] appears in list s with gap constraint g, if
--(ai,a(i+1)) appears in list s with gap constraing g for i=1..k-1.
--
--E.g. "acd" appears in list "axcxyd" with gap constraint 2.
--
--Write a function that, given
--- a list of strings s,
--- gap g,
--- integer k,
--- support n,
--finds the sequences of at most k elements that appear in at least n lists with gap constraing g.
--
--You may leave out subsequences/sublists in the output.
-- E.g. if the output contains "acd" there is no need to output "ac" or "cd".
-- you may also just produce all the sequences of at most k elements that appear in any string with gap constraint g.

import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- | This is the main function.
-- | ["axcxyd","axxcdyd"] -> 2 -> 4 -> 2 -> ["acd","acyd","axcd","axcy","axx","xcyd"]
funn :: [String] -> Int -> Int -> Int -> [String]
funn [] _ _ _ = []
funn s g k n = eliminateSubSequences $ getCommonSequences s g k n


-- | ["abcde","cdefgh","bcdef"] -> 2 -> 4 -> 3 -> ["cd","cde","ce","de"]
getCommonSequences :: [String] -> Int -> Int -> Int -> [String]
getCommonSequences s g k n = map (\(x,_) -> x)
                             $ Map.toList
                             $ Map.filter (\x -> x >= n)
                             $ Map.fromListWith (\x y -> x + y)
                             $ map (\x ->(x,1))
                             $ concat
                             $ map (getAllSequences g k) s

-- | 2 -> 4 -> "abcd" -> ["ab","ac","bc","abc","ad","bd","abd","cd","acd","bcd","abcd"]
getAllSequences :: Int -> Int -> String -> [String]
getAllSequences g k str = nub $ filter (\x -> length x > 1 && length x <= k && respectGap g x str) $ subsequences str


-- | 2 -> "abc" -> "abxxxxc" -> False
-- | 2 -> "abc" -> "abxxc" -> True
respectGap :: Int -> String -> String -> Bool
respectGap _ [] _ = True
respectGap _ [x] _ = True
respectGap g (el1:el2:rest) str = (fromJust $ elemIndex el2 str) - (fromJust $ elemIndex el1 str) - 1 <= g
                                  && respectGap g (el2:rest) str


-- | ["bc","bcd","bd","cd","de","def","df","ef"] -> ["bcd","bd","def","df"]
eliminateSubSequences :: [String] -> [String]
eliminateSubSequences lst = lst \\ filter (\x -> foldl (\acc y -> if isInfixOf x y then True else acc) False (delete x lst)) lst
