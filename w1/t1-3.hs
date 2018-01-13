funn :: [Integer] -> [Integer]
funn lst
    | length lst <= 1 = []
    | lst !! 1 > head lst = head lst : funn (tail lst)
    | otherwise = funn (tail lst)
