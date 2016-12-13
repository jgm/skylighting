import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Highlighting.Kate.Parser

main :: IO ()
main = hspec $ do
  describe "Highlighing.Kate.Parser.Rule" $ do
    it "has a show instance" $ do
      show (DetectChar Dynamic 'a') `shouldBe` "DetectChar Dynamic 'a'"
