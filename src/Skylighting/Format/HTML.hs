module Skylighting.Format.HTML (
      formatHtmlInline
    , formatHtmlBlock
    , styleToCss
    ) where

import Data.List (intersperse)
import qualified Data.Text as Text
import Skylighting.Types
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Format tokens using HTML spans inside @code@ tags. For example,
-- A @KeywordTok@ is rendered as a span with class @kw@.
-- Short class names correspond to 'TokenType's as follows:
-- 'KeywordTok'        = @kw@,
-- 'DataTypeTok'       = @dt@,
-- 'DecValTok'         = @dv@,
-- 'BaseNTok'          = @bn@,
-- 'FloatTok'          = @fl@,
-- 'CharTok'           = @ch@,
-- 'StringTok'         = @st@,
-- 'CommentTok'        = @co@,
-- 'OtherTok'          = @ot@,
-- 'AlertTok'          = @al@,
-- 'FunctionTok'       = @fu@,
-- 'RegionMarkerTok'   = @re@,
-- 'ErrorTok'          = @er@,
-- 'ConstantTok'       = @cn@,
-- 'SpecialCharTok'    = @sc@,
-- 'VerbatimStringTok' = @vs@,
-- 'SpecialStringTok'  = @ss@,
-- 'ImportTok'         = @im@,
-- 'DocumentationTok'  = @do@,
-- 'AnnotationTok'     = @an@,
-- 'CommentVarTok'     = @cv@,
-- 'VariableTok'       = @va@,
-- 'ControlFlowTok'    = @cf@,
-- 'OperatorTok'       = @op@,
-- 'BuiltInTok'        = @bu@,
-- 'ExtensionTok'      = @ex@,
-- 'PreprocessorTok'   = @pp@,
-- 'AttributeTok'      = @at@,
-- 'InformationTok'    = @in@,
-- 'WarningTok'        = @wa@.
-- A 'NormalTok' is not marked up at all.
formatHtmlInline :: FormatOptions -> [SourceLine] -> Html
formatHtmlInline opts = (H.code ! A.class_ (toValue $ Text.unwords
                                                    $ Text.pack "sourceCode"
                                                      : codeClasses opts))
                                . mconcat . intersperse (toHtml "\n")
                                . map (sourceLineToHtml opts)

tokenToHtml :: FormatOptions -> Token -> Html
tokenToHtml _ (NormalTok, txt)  = toHtml txt
tokenToHtml opts (toktype, txt) =
  if titleAttributes opts
     then sp ! A.title (toValue $ show toktype)
     else sp
   where sp = H.span ! A.class_ (toValue $ short toktype) $ toHtml txt

short :: TokenType -> String
short KeywordTok        = "kw"
short DataTypeTok       = "dt"
short DecValTok         = "dv"
short BaseNTok          = "bn"
short FloatTok          = "fl"
short CharTok           = "ch"
short StringTok         = "st"
short CommentTok        = "co"
short OtherTok          = "ot"
short AlertTok          = "al"
short FunctionTok       = "fu"
short RegionMarkerTok   = "re"
short ErrorTok          = "er"
short ConstantTok       = "cn"
short SpecialCharTok    = "sc"
short VerbatimStringTok = "vs"
short SpecialStringTok  = "ss"
short ImportTok         = "im"
short DocumentationTok  = "do"
short AnnotationTok     = "an"
short CommentVarTok     = "cv"
short VariableTok       = "va"
short ControlFlowTok    = "cf"
short OperatorTok       = "op"
short BuiltInTok        = "bu"
short ExtensionTok      = "ex"
short PreprocessorTok   = "pp"
short AttributeTok      = "at"
short InformationTok    = "in"
short WarningTok        = "wa"
short NormalTok         = ""

sourceLineToHtml :: FormatOptions -> SourceLine -> Html
sourceLineToHtml opts cont = mapM_ (tokenToHtml opts) cont

formatHtmlBlockPre :: FormatOptions -> [SourceLine] -> Html
formatHtmlBlockPre opts = H.pre . formatHtmlInline opts

-- | Format tokens as an HTML @pre@ block. If line numbering is
-- selected, this is put into a table row with line numbers in the
-- left cell.  The whole code block is wrapped in a @div@ element
-- to aid styling (e.g. the overflow-x property).  See the
-- documentation for 'formatHtmlInline' for information about how
-- tokens are encoded.
formatHtmlBlock :: FormatOptions -> [SourceLine] -> Html
formatHtmlBlock opts ls = H.div ! A.class_ sourceCode $
                            container ! A.class_ (toValue $ Text.unwords classes)
  where  container = if numberLines opts
                        then H.table $ H.tr ! A.class_ sourceCode $
                                 nums >> source
                        else pre
         sourceCode = toValue "sourceCode"
         classes = Text.pack "sourceCode" :
                   [x | x <- containerClasses opts, x /= Text.pack "sourceCode"]
         pre = formatHtmlBlockPre opts ls
         source = H.td ! A.class_ sourceCode $ pre
         startNum = startNumber opts
         nums = H.td ! A.class_ (toValue "lineNumbers")
                     $ H.pre
                     $ mapM_ lineNum [startNum..(startNum + length ls - 1)]
         lineNum n = if lineAnchors opts
                        then (H.a ! A.id (toValue nStr) ! A.href (toValue $ "#" ++ nStr) $ toHtml $ show n)
                              >> toHtml "\n"
                        else toHtml $ show n ++ "\n"
           where nStr = show n

-- | Returns CSS for styling highlighted code according to the given style.
styleToCss :: Style -> String
styleToCss f = unlines $ divspec ++ tablespec ++ colorspec ++ map toCss (tokenStyles f)
   where colorspec = case (defaultColor f, backgroundColor f) of
                          (Nothing, Nothing) -> []
                          (Just c, Nothing)  -> ["pre, code { color: " ++ fromColor c ++ "; }"]
                          (Nothing, Just c)  -> ["pre, code { background-color: " ++ fromColor c ++ "; }"]
                          (Just c1, Just c2) -> ["pre, code { color: " ++ fromColor c1 ++ "; background-color: " ++
                                                  fromColor c2 ++ "; }"]
         tablespec = [
           "table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {"
          ,"  margin: 0; padding: 0; vertical-align: baseline; border: none; }"
          ,"table.sourceCode { width: 100%; line-height: 100%; " ++
             maybe "" (\c -> "background-color: " ++ fromColor c ++ "; ") (backgroundColor f) ++
             maybe "" (\c -> "color: " ++ fromColor c ++ "; ") (defaultColor f) ++
             "}"
          ,"td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; " ++
             maybe "" (\c -> "background-color: " ++ fromColor c ++ "; ") (lineNumberBackgroundColor f) ++
             maybe "" (\c -> "color: " ++ fromColor c ++ "; ") (lineNumberColor f) ++
             maybe "" (\c -> "border-right: 1px solid " ++ fromColor c ++ "; ") (lineNumberColor f) ++
             "}"
          ,"td.sourceCode { padding-left: 5px; }"
          ]
         divspec = [ "div.sourceCode { overflow-x: auto; }" ]

toCss :: (TokenType, TokenStyle) -> String
toCss (t,tf) = "code > span." ++ short t ++ " { "
                ++ colorspec ++ backgroundspec ++ weightspec ++ stylespec
                ++ decorationspec ++ "} /* " ++ showTokenType t ++ " */"
  where colorspec = maybe "" (\col -> "color: " ++ fromColor col ++ "; ") $ tokenColor tf
        backgroundspec = maybe "" (\col -> "background-color: " ++ fromColor col ++ "; ") $ tokenBackground tf
        weightspec = if tokenBold tf == Just True
                        then "font-weight: bold; "
                        else ""
        stylespec  = if tokenItalic tf == Just True
                        then "font-style: italic; "
                        else ""
        decorationspec = if tokenUnderline tf == Just True
                            then "text-decoration: underline; "
                            else ""
        showTokenType t' = case reverse (show t') of
                             'k':'o':'T':xs -> reverse xs
                             _              -> ""

