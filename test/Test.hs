import Test.Hspec

import Lang.Async
import Lang.CSP
import HackerRank.FunctionProgramming.ReverseAList
import HackerRank.FunctionProgramming.FilterArray

main :: IO ()
main = hspec $ do
  describe "Language Feature Test" $ do
    describe "Async" $ runIO testAsync1
    describe "CSP" $ runIO testCSP
  describe "Hacker Rank" $ do
    describe "Filter Array" $ runIO testFilterArray
    describe "Reverse A List" $ runIO testReverseAList
