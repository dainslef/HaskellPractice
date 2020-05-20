import           Test.Hspec
import           Lang.Async
import           Lang.CSP
import           Lang.DuplicateRecordFields
import           Lang.Exception
import           Lang.FixityDeclaration
import           Lang.GADT
import           Lang.HTTP
import           Lang.Lazy
import           Lang.Map
import           Lang.Maybe
import           Lang.Monad
import           Lang.PatternMatch
import           Lang.RankNType
import           Lang.STM
import           Lang.TChan
import           Lang.TypeClass
import           Lang.TypeFamily

main :: IO ()
main = hspec $ do

  describe "Language Feature Test" $ do
    it "Async"                 testAsync1
    it "CSP"                   testCSP
    it "DuplicateRecordFields" testDuplicateRecordFields
    it "FixityDeclaration"     testFixityDeclaration
    it "GADT"                  testGADT
    it "Http"                  testHttp
    it "Map"                   testCustomMap
    it "Maybe"                 testMaybe
    it "Monad"                 testMonad
    it "PatternMatch"          testPatternMatch
    it "RankNType"             testRankNTypes
    it "STM" $ testSTM1 >> testSTM2
    it "TypeClass" $ do
      testTypeClass1
      testTypeClass2
    it "FunctionalDependencies" $ do
      testFunctionalDependencies1
      testFunctionalDependencies2

  describe "Pending Test" $ do
    xit "Exception" $ do
      testException1
      testException2
      testException3
    xit "TChan" $ testTChan1 >> testTChan2
    xit "Lazy" $ do
      testLazy1
      testLazy2
      testLazy3
      testLazy4
      testLazy5
