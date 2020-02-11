module HackerRank.FunctionProgramming.FilterPositionsInAList where

import Test.HUnit ((@=?))

f :: [Int] -> [Int]
f = k 0 where
  k _ [] = []
  k n (v:l)
    | mod n 2 == 1 = v:next
    | otherwise = next where next = k (n + 1) l

testFilterPositonInAList = [2, 4, 6, 8] @=? f [1, 2, 3, 4, 5, 6, 7, 8, 9]
