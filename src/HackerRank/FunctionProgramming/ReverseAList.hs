module HackerRank.FunctionProgramming.ReverseAList where

import Test.HUnit ((@=?))

rev :: [Int] -> [Int]
rev = f [] where
  f o [] = o
  f o (v:l) = f (v:o) l

testReverseAList = [5, 4, 3, 2, 1] @=? rev [1, 2, 3, 4, 5]