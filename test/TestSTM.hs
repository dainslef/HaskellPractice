import Data.IORef
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Concurrent (threadDelay)

refA, refB :: IO (IORef Int)
refA = newIORef 1000
refB = newIORef 0

varA, varB :: IO (TVar Int)
varA = newTVarIO 1000
varB = newTVarIO 0

changeIORef :: String -> IORef Int -> IORef Int -> IO (Async ())
changeIORef i refA refB = async $ replicateM_ 3 $ do
  modifyIORef' refA (flip (-) 20)
  modifyIORef' refB (+20)
  printIORef i refA refB

changeTVar :: String -> TVar Int -> TVar Int -> IO (Async ())
changeTVar i varA varB = async $ replicateM_ 3 $ do
  atomically $ do
    modifyTVar' varA (flip (-) 20)
    modifyTVar' varB (+20)
  printTVar i varA varB

printIORef :: String -> IORef Int -> IORef Int -> IO ()
printIORef i refA refB = do
  a <- readIORef refA
  b <- readIORef refB
  printAB ("IORef " ++ i ++ ":") a b

printTVar :: String -> TVar Int -> TVar Int -> IO ()
printTVar i varA varB = do
  a <- readTVarIO varA
  b <- readTVarIO varB
  printAB ("TVar " ++ i ++ ":") a b

printAB :: String -> Int -> Int -> IO ()
printAB prefix a b = print $ foldl1 (++)
  [prefix, " A: [", show a, "], B: [", show b, "]"]

test1, test2 :: IO ()
test1 = do
  a <- refA
  b <- refB
  changeIORef "1" a b
  changeIORef "2" a b
  changeIORef "3" a b
  changeIORef "4" a b
  threadDelay 1000000
  printIORef "last" a b
test2 = do
  a <- varA
  b <- varB
  changeTVar "1" a b
  changeTVar "2" a b
  changeTVar "3" a b
  changeTVar "4" a b
  threadDelay 1000000
  printTVar "last" a b

main = test1 >> test2
