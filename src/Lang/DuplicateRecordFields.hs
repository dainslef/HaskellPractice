{-# LANGUAGE DuplicateRecordFields #-}

module Lang.DuplicateRecordFields where

newtype Record1 = Record1
  { name :: String
  }
  deriving (Show, Eq)

newtype Record2 = Record2
  { name :: String
  }
  deriving (Show, Eq)

testDuplicateRecordFields = do
  -- need to specify the type of the expression, or get error: "Ambiguous occurrence ‘name’"
  print $ name (Record1 "23333" :: Record1)
  print $ name (Record2 "23333" :: Record2)
