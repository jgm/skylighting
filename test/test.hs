import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Skylighting.Types

main :: IO ()
main = hspec $ do
  describe "Skylighting.Types.Matcher" $ do
    it "has a show instance" $ do
      show (DetectChar 'a') `shouldBe` "DetectChar 'a'"
