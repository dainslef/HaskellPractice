import CodeWars.Kata.RailFenceCipherEncodingAndDecoding
import CodeWars.Kata.SplitStrings
import Test.Hspec

-- import CodeWars.Kata.UniqueInOrder
-- import CodeWars.Kata.HumanReadableDurationFormat

main :: IO ()
main = hspec $
  describe "Code Wars" $ do
    testSplitStrings
    testRailFenceCipherEncodingAndDecoding

-- testUniqueInOrder
-- testFormatDuration
