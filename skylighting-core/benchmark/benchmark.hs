import Criterion.Main
import Criterion.Types (Config (..))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Skylighting.Core
import System.FilePath
-- import System.Directory

main :: IO ()
main = do
  let inputs = ["abc.haskell", "abc.java", "abc.c", "archive.rhtml",
                "life.lua", "abc.javascript"]
  let getCase fp = do
        let fp' = "test" </> "cases" </> fp
        contents <- readFile fp'
        let format = drop 1 $ takeExtension fp
        return (Text.pack format, Text.pack contents)
  cases <- mapM getCase inputs
  -- xmlfiles <- filter (\x -> takeExtension x == ".xml") <$>
  --                getDirectoryContents "xml"
  let xmlfiles = ["haskell.xml"]
  Right syntaxmap <- loadSyntaxesFromDir "xml"
  defaultMainWith defaultConfig{ timeLimit = 10.0 }
    $ parseBench xmlfiles : map (testBench syntaxmap) cases

parseBench :: [String] -> Benchmark
parseBench xmls =
  bench "parse syntax definitions" $
    nfIO ((Map.size . foldr addSyntaxDefinition mempty) <$> mapM addFile xmls)
   where addFile f = do
           result <- parseSyntaxDefinition ("xml" </> f)
           case result of
                Left e  -> error e
                Right r -> return r

testBench :: SyntaxMap -> (Text, Text) -> Benchmark
testBench syntaxmap (format, contents) =
  bench (Text.unpack format) $ nf
    (sum . map length . either (error "tokenize failed") id .
     tokenize TokenizerConfig{ traceOutput = False
                             , syntaxMap = syntaxmap } syntax) contents

  where syntax = maybe (error "could not find syntax") id
                       (lookupSyntax format syntaxmap)

