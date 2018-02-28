-- Given a list of elements for which a partial order is defined, sort them in the following way: If a is before b in
--  the partial order, then a is before b in the sorted order. If a is not before b and b is not before a, then
--  suppose that there are i elements in the list that are before a in the partial order and j elements that are
--  before b in the partial order. If i < j, then a should be before b in the sorted order.

import Data.List

data Newlean = Truee | Falsee | Undefined deriving (Show,Eq)

-- Here I had to use a wrapper because Haskell was complaining about using String as instance of a typeclass:
--  https://stackoverflow.com/questions/5941701/why-can-i-not-make-string-an-instance-of-a-typeclass
newtype Wrapper = Wrapper String

class PartOrder a where
  (<==) :: a -> a -> Newlean

instance PartOrder Wrapper where
  (Wrapper x) <== (Wrapper y)
     | length x < 1 || length y < 1 = Undefined
     | isInfixOf x y = Truee
     | otherwise = Falsee

-- |Given a list of elements for which a partial order is defined, sorts them
sortPartial :: (PartOrder a) => [a] -> [a]
sortPartial [] = []
sortPartial all@(b:xs) = (sortPartial $ getElemsBefore b xs all) ++ [b] ++ (sortPartial $ getElemsAfter b xs all)

-- If a is before b in the partial order, then a is before b in the sorted order.
-- OR If a is not before b and b is not before a, AND there are i elements in the list that are
--   before a in the partial order and j elements that are before b in the partial order.
--   If i < j, then a should be before b in the sorted order.
getElemsBefore :: (PartOrder a) => a -> [a] -> [a] -> [a]
getElemsBefore b lst all = filter (\a -> a <== b == Truee
                                     || (a <== b == Falsee && b <== a == Falsee
                                         && (length $ filter (\i -> i <== a == Truee) all) <
                                            (length $ filter (\j -> j <== b == Truee) all) )) lst

getElemsAfter :: (PartOrder a) => a -> [a] -> [a] -> [a]
getElemsAfter b lst all = filter (\a -> (a <== b == Falsee && b <== a == Truee)
                                     || (a <== b == Falsee && b <== a == Falsee
                                         && (length $ filter (\i -> i <== a == Truee) all) >=
                                            (length $ filter (\j -> j <== b == Truee) all) )) lst
