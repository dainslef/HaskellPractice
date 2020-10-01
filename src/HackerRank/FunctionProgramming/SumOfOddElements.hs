-- Sum of Odd Elements
-- https://www.hackerrank.com/challenges/fp-sum-of-odd-elements/problem
--
-- You are given a list. Return the sum of odd elements from the given list. The input and output portions will be handled automatically. You need to write a function with the recommended method signature.
--
-- Constraints
--
-- The list will have  elements.
-- Each element will be an integer  where .
--
-- Sample Input
--
-- 3
-- 2
-- 4
-- 6
-- 5
-- 7
-- 8
-- 0
-- 1
-- Sample Output
--
-- 16
-- Explanation
--
-- Sum of odd elements is
--
-- Method Signature
--
-- Number Of Parameters: 1
-- Parameters: [list]
-- Returns: Number
-- For Hackers Using Clojure
--
-- This will be the outline of your function body (fill in the blank portion marked by underscores):
--
--  (fn[lst]___________________________)
-- For Hackers Using Scala
--
-- This will be the outline of your function body (fill in the blank portion marked by underscores):
--
--  def f(arr:List[Int]):Int = __________________
-- For Hackers Using Haskell
--
-- This will be the outline of your function body (fill in the blank portion marked by underscores):
--
--  f arr = ______________________
-- For Hackers Using other Languages
--
-- You have to read input from standard input and write output to standard output. Please follow the input/output format mentioned above.
--
-- NOTE: You only need to submit the code above after filling in the blanks appropriately. The input and output section will be handled by us. The focus is on writing the correct function.

module HackerRank.FunctionProgramming.SumOfOddElements where

import Test.Hspec

f :: Integral a => [a] -> a
f = sum . filter (\v -> mod v 2 == 1) -- f = sum . filter ((== 1) . flip mod 2)

test :: Spec
test = it "SumOfOddElements" $ do
  f [1, 2, 3, 4, 5] `shouldBe` 9
  f [3, 2, 4, 6, 5, 7, 8, 0, 1] `shouldBe` 16
