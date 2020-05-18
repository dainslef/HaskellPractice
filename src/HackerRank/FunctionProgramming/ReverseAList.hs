module HackerRank.FunctionProgramming.ReverseAList where

import           Test.Hspec

rev :: [Int] -> [Int]
rev = f [] where
  f o []      = o
  f o (v : l) = f (v : o) l

testReverseAList :: SpecWith ()
testReverseAList =
  it "ReverseAList" $ rev [1, 2, 3, 4, 5] `shouldBe` [5, 4, 3, 2, 1]
