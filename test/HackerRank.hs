import           Test.Hspec
import           HackerRank.FunctionProgramming.ReverseAList
import           HackerRank.FunctionProgramming.FilterArray
import           HackerRank.FunctionProgramming.FilterPositionsInAList

main :: IO ()
main = hspec $ describe "Hacker Rank" $ do
  testFilterArray
  testReverseAList
  testFilterPositionInAList
