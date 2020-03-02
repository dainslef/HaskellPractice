import Test.Hspec

import Lang.Async
import Lang.CSP
import Lang.TypeClass
import HackerRank.FunctionProgramming.ReverseAList
import HackerRank.FunctionProgramming.FilterArray

main :: IO ()
main = hspec $ do
  describe "Language Feature Test" $ do
    it "Async" testAsync1
    it "CSP" testCSP
    it "FunctionalDependencies" testTypeClass4
  describe "Hacker Rank" $ do
    it "Filter Array" testFilterArray
    it "Reverse A List" testReverseAList
