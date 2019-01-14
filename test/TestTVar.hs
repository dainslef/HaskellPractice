import Control.Monad (replicateM_)
import Control.Monad.STM (atomically)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar

varA, varB :: IO (TVar Int)
varA = newTVarIO 1000
varB = newTVarIO 0

changeTVar :: String -> TVar Int -> TVar Int -> IO (Async ())
changeTVar i varA varB = async $ replicateM_ 2 $ do
  (a, b) <- atomically $ do
    a <- readTVar varA
    b <- readTVar varB
    let (na, nb) = (a - 20, b + 20)
    writeTVar varA na
    writeTVar varB nb
    return (na, nb)
  threadId <- myThreadId
  printValue ("TVar " ++ i ++ " [" ++ (show threadId) ++ "]:") a b

printValue :: String -> Int -> Int -> IO ()
printValue prefix a b = print $ foldl1 (++)
  [prefix, " A: [", show a, "], B: [", show b, "]"]

main :: IO ()
main = do
  a <- varA
  b <- varB
  changeTVar "1" a b
  changeTVar "2" a b
  changeTVar "3" a b
  changeTVar "4" a b
  changeTVar "5" a b
  threadDelay 500000
