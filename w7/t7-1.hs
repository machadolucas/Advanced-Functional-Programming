-- Write a Haskell program that creates two MVars, stores a String in the first one, and then starts a thread.
-- The thread that adds "hello, " in the front of that string and stores the result in the second MVar.
-- Then the program reads the modified string and prints it out.

import Control.Concurrent

main = do
  putStrLn "Tell me your name:"
  line <- getLine
  m1 <- newMVar (line)
  m2 <- newEmptyMVar
  forkIO $ do
    v <- takeMVar m1
    putMVar m2 $ ("hello, " ++ v)
  result <- takeMVar m2
  putStrLn $ result
