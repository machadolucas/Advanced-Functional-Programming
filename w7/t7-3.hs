-- Write a Haskell program that creates two threads and gives them both a list of integers.
-- Then the threads start taking turns, sending each other at one time one integer from the list.
-- If the receiver receives an integer that is in its original list of integers, it sends information
-- about that to the sender. Then both will terminate. Use MVars for communication.

import Control.Concurrent

main = do
  let t1List = [1,2,3,5,7,8,9,10]
  let t2List = [7,6,5,2,1,4,6,5]
  toT1 <- newEmptyMVar
  toT2 <- newEmptyMVar
  endVar <- newEmptyMVar
  forkIO $ do
     putMVar toT1 (head t2List)
     threadLoop t1List (tail t1List) endVar toT1 toT2
  forkIO $ do
     putMVar toT2 (head t1List)
     threadLoop t2List (tail t2List) endVar toT2 toT1
  quit <- takeMVar endVar
  putStrLn $ "done."


threadLoop :: [Integer] -> [Integer] -> MVar Bool -> MVar Integer -> MVar Integer -> IO ()
threadLoop originalList list endVar inVar outVar = do
  value <- takeMVar inVar
  shouldEnd <- isEmptyMVar endVar
  if shouldEnd || length list < 1 || value `elem` originalList
  then do
     putStrLn $ "finalizing thread."
     putMVar endVar (True)
  else do
     putMVar outVar (head list)
     threadLoop originalList (tail list) endVar inVar outVar
