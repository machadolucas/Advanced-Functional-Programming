
-- | DiffList type. It instances Show so it shows as a list, but operations such as `mappend` are done with actual DiffList types.
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance (Show a) => Show (DiffList a) where
  show x = show (fromDiffList x)
instance (Read a) => Read (DiffList a) where
  readsPrec _ txt = [(toDiffList $ read txt, "")]

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))


-- | Key and list of values. a is key type, b is value type.
data NodeData a b = Klv {key :: a , val :: DiffList b} deriving (Show, Read)

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
    | k == key a = Node (Klv k $ val a `mappend` v) left right
    | k < key a  = Node a (treeInsert (Klv k v) left) right
    | k > key a  = Node a left (treeInsert (Klv k v) right)

-- Example of use:
-- let myTree = treeInsert (Klv 2 (toDiffList [8,3,1])) (treeInsert (Klv 2 (toDiffList [3,5,4])) (treeInsert (Klv 3 (toDiffList [1,2,3])) EmptyTree))
-- Node (Klv {key = 3, val = [1,2,3]}) (Node (Klv {key = 2, val = [3,5,4,8,3,1]}) EmptyTree EmptyTree) EmptyTree

treeSearch :: (Ord a) => a -> Tree a b -> DiffList b
treeSearch _ EmptyTree = toDiffList []
treeSearch k (Node a left right)
    | k == key a = val a
    | k < key a  = treeSearch k left
    | k > key a  = treeSearch k right
