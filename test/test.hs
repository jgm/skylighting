import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Highlighting.Kate.Parser

main :: IO ()
main = hspec $ do
  describe "Highlighing.Kate.Parser.Matcher" $ do
    it "has a show instance" $ do
      show (DetectChar 'a') `shouldBe` "DetectChar 'a'"
