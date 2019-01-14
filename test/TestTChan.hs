import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan

main = test2

test1 :: IO ()
test1 = do

  c <- chan
  a <- sendMessage c
  receiveMessage c
  receiveMessage c
  receiveMessage c
  wait a

  where

    chan :: IO (TChan String)
    chan = newBroadcastTChanIO

    sendMessage :: TChan String -> IO (Async ())
    sendMessage chan = async $ forever $ do
      input <- getLine
      print $ "Input: " ++ input
      atomically $ writeTChan chan input

    receiveMessage :: TChan String -> IO (Async ())
    receiveMessage chan = async $ do
      rChan <- atomically $ dupTChan chan
      threadId <- myThreadId
      forever $ do
        receive <- atomically $ readTChan rChan
        print $ "Receive [" ++ (show threadId) ++ "]: " ++ receive

test2 :: IO ()
test2 = do

  c <- chan
  a <- sendMessage c
  receiveMessage c
  receiveMessage c
  receiveMessage c
  wait a

  where

    chan :: IO (TChan String)
    chan = newTChanIO

    sendMessage :: TChan String -> IO (Async ())
    sendMessage chan = async $ forever $ do
      input <- getLine
      print $ "Input: " ++ input
      atomically $ writeTChan chan input

    receiveMessage :: TChan String -> IO (Async ())
    receiveMessage chan = async $ do
      threadId <- myThreadId
      forever $ do
        receive <- atomically $ readTChan chan
        print $ "Receive [" ++ (show threadId) ++ "]: " ++ receive
