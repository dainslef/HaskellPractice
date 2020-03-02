{-# LANGUAGE LambdaCase #-}

module Lang.Exception where

import Control.Exception
import Control.Monad (void)

newtype MyException = MyException String deriving Show
instance Exception MyException

testException1, testException2, testException3 :: IO ()

testException1 = try (do
  let n1 = 1
  let n2 = error "Try Exception!"
  print "Run..."
  return $ n1 + n2) >>= \case
    Left (SomeException e) -> print $ "Exception info: " ++ displayException e
    Right a -> print $ "Success: " ++ show a

testException2 = do

  re1 <- catch (doSomething e1) dealException
  print $ "Result1: " ++ show re1

  re2 <- handle dealMyException $ doSomething e2
  print $ "Result2: " ++ show re2

  re3 <- handle dealMyException $ doSomething e1
  print $ "Result3: " ++ show re3

  where

    e1, e2 :: Int
    e1 = error "Error!"
    e2 = throw $ MyException "MyException!"

    doSomething :: Int -> IO Int
    doSomething e = do
      let n1 = 1
      let n2 = e
      print "Run..."
      return $! n1 + n2

    dealException :: SomeException -> IO Int
    dealException = showException

    dealMyException :: MyException -> IO Int
    dealMyException = showException

    showException :: Exception e => e -> IO Int
    showException = (>> return 0) . putStrLn . ("Catch the exception: "++) . displayException

testException3 = do

  _ <- onException (doSomething 1) after
  -- _ <- onException (error "onException") after
  bracket_ before after (void $ doSomething $ error "bracket_")

  where

    doSomething :: Int -> IO Int
    doSomething v = do
      let n1 = 1
      let n2 = v
      print $ "Run [" ++ show v ++ "]..."
      return $! n1 + n2

    before, after :: IO ()
    before = print "Before action..."
    after = print "After action..."
