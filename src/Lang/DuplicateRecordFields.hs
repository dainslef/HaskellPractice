{-# LANGUAGE DuplicateRecordFields #-}

module Lang.DuplicateRecordFields where

data Record1 = Record1 {
  name :: String
} deriving (Show, Eq)

data Record2 = Record2 {
  name :: String
} deriving (Show, Eq)

testDuplicateRecordFields = do
  print $ name ((Record1 "23333") :: Record1) -- 必须显式指定参数类型，否则会得到
  print $ name ((Record2 "23333") :: Record2)
