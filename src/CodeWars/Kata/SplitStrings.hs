-- 6 kyu
-- Split Strings
-- https://www.codewars.com/kata/515de9ae9dcfc28eb6000001/train/haskell
--
-- Complete the solution so that it splits the string into pairs of two characters. If the string contains an odd number of characters then it should replace the missing second character of the final pair with an underscore ('_').
--
-- Examples:
--
-- solution "abc" `shouldBe` ["ab", "c_"]
-- solution "abcdef" `shouldBe` ["ab", "cd", "ef"]

module CodeWars.Kata.SplitStrings where

import Test.Hspec

splitStrings :: String -> [String]
splitStrings = f []
  where
    f :: [String] -> String -> [String]
    f out (c1 : c2 : others) = f (out ++ [c1 : [c2]]) others
    f out [c] = out ++ [c : ['_']]
    f out [] = out

testSplitStrings :: Spec
testSplitStrings = it "SplitStrings" $ do
  splitStrings "abc" `shouldBe` ["ab", "c_"]
  splitStrings "abcdef" `shouldBe` ["ab", "cd", "ef"]
