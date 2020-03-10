import Test.Hspec

import Lang.Async
import Lang.CSP
import Lang.TypeClass
import Lang.FixityDeclaration
import HackerRank.FunctionProgramming.ReverseAList
import HackerRank.FunctionProgramming.FilterArray

main :: IO ()
main = hspec $ do
  describe "Language Feature Test" $ do
    it "Async" testAsync1
    it "CSP" testCSP
    it "FunctionalDependencies" testTypeClass4
    it "FixityDeclaration" testFixityDeclaration
  describe "Hacker Rank" $ do
    it "Filter Array" testFilterArray
    it "Reverse A List" testReverseAList
