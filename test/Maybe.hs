{-# LANGUAGE KindSignatures #-}

import Data.Maybe

num1, num2, num3 :: Maybe Int
num1 = Just 2333
num2 = Nothing
num3 = Just 666

main :: IO ()
main = do

  print $ "Num0: " ++ (show $ fromMaybe 0 re0)
  print $ "Num1: " ++ (show $ fromMaybe 0 re1)
  print $ testMaybe "TestString" 2.0

  where

    re0 = do
      n1 <- num1
      n2 <- num2
      return $ n1 + n2

    re1 = do
      n1 <- num1
      n3 <- num3
      return $ n1 + n3

    -- Maybe Monad
    -- 数值转换使用 fromIntegral 函数
    testMaybe :: (Foldable a, Num b) => a c -> b -> Maybe b
    testMaybe a b = do
      s <- Just a
      Just $ (fromIntegral $ length s) + b
