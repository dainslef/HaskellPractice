import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (mapM)

actionN :: Int -> IO String
actionN num = do
  threadDelay 1000000 -- 暂停线程 1s，便于观察异步操作执行情况
  let n = show num
  print $ "Running action" ++ n ++ "..."
  return $ "Action " ++ n

actions, actionsL, actionsM :: [IO a] -> IO [Async a]
actions = foldr (\a b -> do
  l <- b
  c <- async a
  return $ c : l) $ return []
actionsL = foldr (\a b ->
  async a >>= \c ->
  b >>= return . (c:)) $ return []
actionsM = mapM async

main1, main2 :: IO ()

main1 = do
  res <- withAsync (actionN 1) $ \async1 -> do
    print "With Async..."
    res <- wait async1
    return res
  actions <- actionsM [actionN 2, actionN 3]
  (as, s) <- waitAny actions
  print $ "First finish actions: " ++ s
  print $ "End: " ++ (show res)

main2 = do
  a1 <- async $ actionN 1
  print "After action1"
  a2 <- async $ actionN 2
  print "After action2"
  (r1, r2) <- waitBoth a1 a2
  print $ "Finish: " ++ r1 ++ " " ++ r2

main :: IO ()
main = main2
