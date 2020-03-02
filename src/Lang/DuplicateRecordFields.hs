{-# LANGUAGE DuplicateRecordFields #-}

module Lang.DuplicateRecordFields where

newtype Record1 = Record1 {
  name :: String
} deriving (Show, Eq)

newtype Record2 = Record2 {
  name :: String
} deriving (Show, Eq)

testDuplicateRecordFields = do
  print $ name (Record1 "23333" :: Record1) -- 必须显式指定参数类型，否则会得到異常： Ambiguous occurrence ‘name’
  print $ name (Record2 "23333" :: Record2)
