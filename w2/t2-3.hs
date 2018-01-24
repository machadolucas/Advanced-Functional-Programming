-- |Receives a list and a comparison function to quicksort that list. The comparison function should return LT, GT or EQ.
sort' :: (Ord a) => [a] -> (a -> a -> Ordering) -> [a]
sort' [] = []
sort' (x:xs) f = sort' [y | y <- xs, (f y x == LT || f y x == ET)] ++ [x] ++ sort' [y | y <- xs, f y x == GT]
