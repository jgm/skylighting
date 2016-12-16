import Skylighting.Tokenizer
import Skylighting.Syntax
import Text.Show.Pretty
import System.Environment
import qualified Data.Map as Map

main :: IO ()
main = do
  [lang] <- getArgs
  syn <- case Map.lookup lang syntaxMap of
                Nothing -> error "language not found"
                Just s  -> return s
  getContents >>= putStrLn . ppShow . tokenize syn
