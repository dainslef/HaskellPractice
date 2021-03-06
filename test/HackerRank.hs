import HackerRank.FunctionProgramming.FilterArray as FilterArray
import HackerRank.FunctionProgramming.FilterPositionsInAList as FilterPositionsInAList
import HackerRank.FunctionProgramming.ReverseAList as ReverseAList
import HackerRank.FunctionProgramming.SumOfOddElements as SumOfOddElements
import HackerRank.FunctionProgramming.UpdateList as UpdateList
import Test.Hspec

-- import HackerRank.FunctionProgramming.EvaluatingEX as EvaluatingEX
-- import HackerRank.FunctionProgramming.ListLength as ListLength

main :: IO ()
main = hspec $
  describe "Hacker Rank" $ do
    FilterArray.test
    FilterPositionsInAList.test
    ReverseAList.test
    SumOfOddElements.test
    UpdateList.test

-- ListLength.test
-- EvaluatingEX.test
