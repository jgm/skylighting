{-# LANGUAGE OverloadedStrings #-}

import Skylighting.Tokenizer
import Skylighting.Syntax
import Text.Show.Pretty
import System.Environment
import qualified Data.Map as Map
import Skylighting.Format.HTML
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Skylighting.Types
import Skylighting.Styles

main :: IO ()
main = do
  (lang:rest) <- getArgs
  let htmlOutput = "--html" `elem` rest
  syn <- case Map.lookup lang syntaxMap of
                Nothing -> error "language not found"
                Just s  -> return s
  res <- tokenize syn <$> getContents
  case res of
      Left e -> error e
      Right toks | htmlOutput -> putStrLn $
        renderHtml $ do H.head $
                          do H.meta H.! A.charset "utf-8"
                             H.style H.! A.type_ "text/css" $ H.toHtml
                                $ styleToCss pygments
                        H.body (formatHtmlBlock defaultFormatOpts toks)
                 | otherwise -> putStrLn $ ppShow toks
