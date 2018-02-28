-- Consider a tree storing value lists for pairs. Suppose that the value type belongs to Ord. Make tree a Monoid.
--EDIT: No need for any other pairs, just a (key,[value]) pair like before.

-- | Key and list of values. a is key type, b is value type.
data NodeData a b = Klv {key :: a , val :: [b]} deriving (Show, Read)

instance (Ord a) => Monoid (Tree a b) where
    mempty = EmptyTree
    mappend EmptyTree tree = tree
    mappend tree EmptyTree = tree
    t1 `mappend` (Node a2 left2 right2) = (treeInsert a2 t1) `mappend` left2 `mappend` right2

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

treeSearch :: (Ord a) => a -> Tree a b -> [b]
treeSearch _ EmptyTree = []
treeSearch k (Node a left right)
    | k == key a = val a
    | k < key a  = treeSearch k left
    | k > key a  = treeSearch k right

-- Example of use:
-- let myTree = treeInsert (Klv 2 [(8,8),(3,3),(1,1)]) (treeInsert (Klv 2 [(3,3),(5,5),(4,4)]) (treeInsert (Klv 3 [(1,1),(2,2),(3,3)]) EmptyTree))
-- Node (Klv {key = 3, val = [(1,1),(2,2),(3,3)]}) (Node (Klv {key = 2, val = [(3,3),(5,5),(4,4),(8,8),(3,3),(1,1)]}) EmptyTree EmptyTree) EmptyTree

-- let myTree2 = (treeInsert (Klv 3 [(1,1),(2,2),(3,3)]) EmptyTree)
-- myTree2 `mappend` myTree2
-- Node (Klv {key = 3, val = [(1,1),(2,2),(3,3),(1,1),(2,2),(3,3)]}) EmptyTree EmptyTree
