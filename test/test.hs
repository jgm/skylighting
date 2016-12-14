import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Skylighting.Parser

main :: IO ()
main = hspec $ do
  describe "Skylighting.Parser.Matcher" $ do
    it "has a show instance" $ do
      show (DetectChar 'a') `shouldBe` "DetectChar 'a'"
