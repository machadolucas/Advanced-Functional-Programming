-- Make a function to delete (key,value) pairs from the Tree of Task 3.3. If the key does not exist, nothing is done.
-- If the key exists and the value not, nothing is done. If the key exists and the value is not the last value for
-- that key, then the value is removed from the list for that key. If the key exists and the value is the last value
-- for the key, then if the node has no children, it is replaced by EmptyTree, otherwise either left or right node
-- is "lifted" to its position.

import qualified Data.Map as Mp
import Data.List

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

-- | Deletes (key,value) pairs from a Tree.
--   If the key does not exist, nothing is done. If the key exists and the value not, nothing is done. If the key
--   exists and the value is not the last value for that key, then the value is removed from the list for that key.
--   If the key exists and the value is the last value for the key, then if the node has no children, it is replaced by
--   EmptyTree, otherwise either left or right node is "lifted" to its position
treeDelValue :: (Ord a, Eq b) => (a, b) -> Tree a b -> Tree a b
treeDelValue _ EmptyTree = EmptyTree
treeDelValue (k, v) (Node a left right)
    | k == key a && v `notElem` val a = Node a left right
    | k == key a && length (val a) > 1 = Node (Klv k $ delete v (val a)) left right
    | k == key a && left == EmptyTree && right == EmptyTree = EmptyTree
    | k == key a && left == EmptyTree && right /= EmptyTree = right
    | k == key a && left /= EmptyTree && right == EmptyTree = left
    | k == key a = Node (Klv (key (getNodeData left)) $ val (getNodeData left)) (getNodeLeft left) (getNodeRight left)
    | k < key a  = Node a (treeDelValue (k, v) left) right
    | k > key a  = Node a left (treeDelValue (k, v) right)

getNodeData :: Tree a b -> NodeData a b
getNodeData (Node x _ _) = x

getNodeLeft :: Tree a b -> Tree a b
getNodeLeft (Node _ x _) = x

getNodeRight :: Tree a b -> Tree a b
getNodeRight (Node _ _ x) = x
