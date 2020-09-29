module HackerRank.FunctionProgramming.FilterArray where

import Test.Hspec

f :: Int -> [Int] -> [Int]
f _ [] = []
f n (v : l)
  | v < n = v : next
  | otherwise = next
  where
    next = f_ n l
    -- use tail recursion
    f_ :: Int -> [Int] -> [Int]
    f_ = k []
      where
        k o _ [] = o
        k o n (v : l)
          | v < n = k (o ++ [v]) n l
          | otherwise = k o n l

test :: Spec
test = it "FilterArray" $ f 3 [1, 2, 3, 4, 5, 6, 7, 8, 9] `shouldBe` [1, 2]
