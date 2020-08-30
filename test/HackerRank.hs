import HackerRank.FunctionProgramming.FilterArray
import HackerRank.FunctionProgramming.FilterPositionsInAList
import HackerRank.FunctionProgramming.ReverseAList
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Hacker Rank" $ do
    testFilterArray
    testReverseAList
    testFilterPositionInAList
