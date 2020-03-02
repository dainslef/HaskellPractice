module Lang.TChan where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan

sendMessage :: TChan String -> IO (Async ())
sendMessage chan = async $ forever $ do
  input <- getLine
  print $ "Input: " ++ input
  atomically $ writeTChan chan input

testTChan1 = do

  c <- chan
  a <- sendMessage c
  receiveMessage c
  receiveMessage c
  receiveMessage c
  wait a

  where

    chan :: IO (TChan String)
    chan = newBroadcastTChanIO

    receiveMessage :: TChan String -> IO (Async ())
    receiveMessage chan = async $ do
      rChan <- atomically $ dupTChan chan
      threadId <- myThreadId
      forever $ do
        receive <- atomically $ readTChan rChan
        print $ "Receive [" ++ show threadId ++ "]: " ++ receive

testTChan2 = do

  c <- chan
  a <- sendMessage c
  receiveMessage c
  receiveMessage c
  receiveMessage c
  wait a

  where

    chan :: IO (TChan String)
    chan = newTChanIO

    receiveMessage :: TChan String -> IO (Async ())
    receiveMessage chan = async $ do
      threadId <- myThreadId
      forever $ do
        receive <- atomically $ readTChan chan
        print $ "Receive [" ++ show threadId ++ "]: " ++ receive
        threadDelay 500000
