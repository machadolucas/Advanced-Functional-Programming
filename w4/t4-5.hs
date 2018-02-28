-- Add exception management in the solution for Task 4.3.

import System.Directory
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Data.List

-- | Key and list of values. a is key type, b is value type.
data NodeData = Klv {key :: Int
                        , val :: [Int]
                        } deriving (Show, Read)

-- | Equals and ordering based on key
instance Eq (NodeData) where
    x == y = (key x) == (key y)
instance Ord (NodeData) where
  x `compare` y = (key x) `compare` (key y)

-- | Tree definition
data Tree = EmptyTree | Node NodeData Tree Tree deriving (Show, Read, Eq)

getNodeData :: Tree -> NodeData
getNodeData (Node x _ _) = x

getNodeLeft :: Tree -> Tree
getNodeLeft (Node _ x _) = x

getNodeRight :: Tree -> Tree
getNodeRight (Node _ _ x) = x

-- | read filename    -- reads a tree from the file
--   write filename    -- writes the tree in the memory to the file
--   insert key value   -- insert a (key, value) pair to the tree
--   delete key  -- delete the entry with the given key from the tree
--   quit -- terminate the program.

-- Since Haskell does not have variables, by "tree in memory" I interpreted "in a temp file".

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("read", readTree)
            , ("write", writeTree)
            , ("insert", insertTree)
            , ("delete", deleteTree)
            ]

main = toTry `catch` handler

toTry :: IO ()
toTry = do
    commands <- getLine
    let command = head $ words commands
    let args = tail $ words commands
    if command /= "quit" then do
        let (Just action) = lookup command dispatch
        tree <- action $ args
        print tree
        main
    else do
        tempExists <- doesFileExist "temp"
        if tempExists then do
            removeFile "temp"
        else do
            return ()

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e

readTree :: [String] -> IO ()
readTree [filename] = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle contents
    let tree = readT contents
    putStrLn $ "Read: " ++ (show tree)
    hClose handle
    hClose tempHandle
    renameFile tempName "temp"

writeTree :: [String] -> IO ()
writeTree [filename] = do
    handle <- openFile "temp" ReadMode
    contents <- hGetContents handle
    (tempName, tempHandle) <- openTempFile "." filename
    hPutStr tempHandle contents
    putStrLn $ "Tree saved in " ++ filename
    hClose handle
    hClose tempHandle
    renameFile tempName filename

insertTree :: [String] -> IO ()
insertTree (k:v:xs) = do
    handle <- openFile "temp" ReadMode
    contents <- hGetContents handle
    (tempName, tempHandle) <- openTempFile "." "temp2"
    let tree = readT contents
    let key = read k :: Int
    let val = read v :: Int
    let updatedTree = treeInsert (Klv key $ val : []) tree
    hPutStr tempHandle $ show updatedTree
    putStrLn $ "Updated tree: " ++ (show updatedTree)
    hClose handle
    hClose tempHandle
    removeFile "temp"
    renameFile tempName "temp"

deleteTree :: [String] -> IO ()
deleteTree (k:xs) = do
    handle <- openFile "temp" ReadMode
    contents <- hGetContents handle
    (tempName, tempHandle) <- openTempFile "." "temp2"
    let tree = readT contents
    let key = read k :: Int
    let updatedTree = treeDelKey key tree
    hPutStr tempHandle $ show updatedTree
    putStrLn $ "Updated tree: " ++ (show updatedTree)
    hClose handle
    hClose tempHandle
    removeFile "temp"
    renameFile tempName "temp"

-- Pure-only functions below

singleton :: NodeData -> Tree
singleton (Klv k v) = Node (Klv k v) EmptyTree EmptyTree

treeInsert :: NodeData -> Tree -> Tree
treeInsert (Klv k v) EmptyTree = singleton (Klv k v)
treeInsert (Klv k v) (Node a left right)
    | k == key a = Node (Klv k $ val a ++ v) left right
    | k < key a  = Node a (treeInsert (Klv k v) left) right
    | k > key a  = Node a left (treeInsert (Klv k v) right)

treeDelKey :: Int -> Tree -> Tree
treeDelKey _ EmptyTree = EmptyTree
treeDelKey k (Node a left right)
    | k == key a && left == EmptyTree && right == EmptyTree = EmptyTree
    | k == key a && left == EmptyTree && right /= EmptyTree = right
    | k == key a && left /= EmptyTree && right == EmptyTree = left
    | k == key a = Node (Klv (key (getNodeData left)) $ val (getNodeData left)) (getNodeLeft left) (getNodeRight left)
    | k < key a  = Node a (treeDelKey k left) right
    | k > key a  = Node a left (treeDelKey k right)


readT :: String -> Tree
readT [] = EmptyTree
readT str = read str
