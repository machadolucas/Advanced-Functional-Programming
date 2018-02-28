-- Modify the Tree datatype in the book in such a way that each node stores a key and a list of values.
-- When a new value with an existing key is inserted, the value is just added to the list. If a new key
-- is introduced, then the key is stored in a new node and the list at this point just contains the
-- value to be inserted. Seaching by a key returns the list of values.

import qualified Data.Map as Mp

-- | Key and list of values. a is key type, b is value type.
data NodeData a b = Klv {key :: a
                        , val :: [b]
                        } deriving (Show, Read)

-- | Equals and ordering based on key
instance (Eq a) => Eq (NodeData a b) where
    x == y = (key x) == (key y)
instance (Ord a) => Ord (NodeData a b) where
  x `compare` y = (key x) `compare` (key y)

-- | Tree definition
data Tree a b = EmptyTree | Node (NodeData a b) (Tree a b) (Tree a b) deriving (Show, Read, Eq)

singleton :: NodeData a b -> Tree a b
singleton (Klv k v) = Node (Klv k v) EmptyTree EmptyTree

treeInsert :: (Ord a) => NodeData a b -> Tree a b -> Tree a b
treeInsert (Klv k v) EmptyTree = singleton (Klv k v)
treeInsert (Klv k v) (Node a left right)
    | k == key a = Node (Klv k $ val a ++ v) left right
    | k < key a  = Node a (treeInsert (Klv k v) left) right
    | k > key a  = Node a left (treeInsert (Klv k v) right)

-- Example of use:
-- let myTree = treeInsert (Klv 2 [8,3,1]) (treeInsert (Klv 2 [3,5,4]) (treeInsert (Klv 3 [1,2,3]) EmptyTree))
-- Node (Klv {key = 3, val = [1,2,3]}) (Node (Klv {key = 2, val = [3,5,4,8,3,1]}) EmptyTree EmptyTree) EmptyTree

treeSearch :: (Ord a) => a -> Tree a b -> [b]
treeSearch _ EmptyTree = []
treeSearch k (Node a left right)
    | k == key a = val a
    | k < key a  = treeSearch k left
    | k > key a  = treeSearch k right
