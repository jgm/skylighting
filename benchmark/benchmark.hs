import Skylighting
import Criterion.Main
import Criterion.Types (Config(..))
import System.FilePath
import System.Directory
import Data.Text (Text)
import qualified Data.Text as Text

main :: IO ()
main = do
  -- inputs <- filter (\fp -> take 1 fp /= ".")
  --       <$> getDirectoryContents ("test" </> "cases")
  let inputs = ["abc.haskell", "abc.java", "abc.c", "archive.rhtml",
                "life.lua", "abc.javascript"]
  let getCase fp = do
        let fp' = "test" </> "cases" </> fp
        contents <- readFile fp'
        let format = drop 1 $ takeExtension fp
        return (Text.pack format, Text.pack contents)
  cases <- mapM getCase inputs
  defaultMainWith defaultConfig{ timeLimit = 10.0 }
    $ map testBench cases

testBench :: (Text, Text) -> Benchmark
testBench (format, contents) =
  bench (Text.unpack format) $ nf
    (sum . map length . either (error "tokenize failed") id .
     tokenize TokenizerConfig{ traceOutput = False
                             , syntaxMap = defaultSyntaxMap } syntax) contents

  where syntax = maybe (error "could not find syntax") id
                       (lookupSyntax format defaultSyntaxMap)

