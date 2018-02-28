-- Implement Splay Tree (see e.g. https://en.wikipedia.org/wiki/Splay_tree)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Show)
data Direction = LeftH | RightH deriving (Eq, Show)

-- Main function that splays the tree. Receives an element of the tree and a tree.
splay :: (Ord a) => a -> Tree a -> Tree a
splay a tree = reorder $ path a tree [(undefined, tree)]
    where path a EmptyTree ps = ps
          path a (Node b left right) ps =
              case compare a b of
                  LT -> path a left $ (LeftH, left) : ps
                  GT -> path a right $ (RightH, right) : ps
                  EQ -> ps

-- Inserts an element in a tree by value order (does not splay)
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert elem Empty          = Node elem Empty Empty
treeInsert elem node@(Node val left right) = case compare elem val of
                            EQ -> node -- Returns the same node if already existent
                            LT -> Node val (treeInsert elem left) right
                            GT -> Node val left (treeInsert elem right)

-- zig steps, for left and right
zigLeft (Node x a b) (Node y _ c) = Node x a (Node y b c)
zigRight (Node x a b) (Node y c _) = Node x (Node y c a) b

-- zig-zig steps, for left and right
zigzigLeft (Node x a b) (Node y _ c) (Node g _ d) = Node x a (Node y b (Node g c d))
zigzigRight (Node x a b) (Node y c _) (Node g d _) =  Node x (Node y (Node g d c) a) b

-- zig-zag steps, for left and right
zigzagLeft (Node x b c) (Node y a _) (Node g _ d) = Node x (Node y a b) (Node g c d)
zigzagRight (Node x b c) (Node y _ a) (Node g d _) = Node x (Node g d b) (Node y c a)


-- Reorders the tree
reorder :: (Ord a) => [(Direction, Tree a)] -> Tree a
reorder ((_, tree):[]) = tree
reorder ((LeftH, x):(_, y):[]) = zigLeft x y
reorder ((RightH, x):(_, y):[]) = zigRight x y
reorder ((LeftH, x):(LeftH, y):(z,g):ps) = reorder $ (z, zigzigLeft x y g):ps
reorder ((RightH, x):(RightH, y):(z,g):ps) = reorder $ (z, zigzigRight x y g):ps
reorder ((RightH, x):(LeftH, y):(z,g):ps) = reorder $ (z, zigzagLeft x y g):ps
reorder ((LeftH, x):(RightH, y):(z,g):ps) = reorder $ (z, zigzagRight x y g):ps
