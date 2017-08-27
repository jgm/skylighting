module Skylighting.Format.HTML (
      formatHtmlInline
    , formatHtmlBlock
    , styleToCss
    ) where

import Data.List (intersperse)
import Data.String (fromString)
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
                                . zipWith (sourceLineToHtml opts) [startNum..]
   where startNum = LineNo $ startNumber opts

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

sourceLineToHtml :: FormatOptions -> LineNo -> SourceLine -> Html
sourceLineToHtml opts lno cont = wrapElement ! A.class_ sourceLine
                                       ! A.id lineNum
                                       ! A.href lineRef
                                       ! H.dataAttribute (fromString "line-number") lineNum $
                                mapM_ (tokenToHtml opts) cont
  where  sourceLine = toValue "sourceLine"
         lineNum = toValue . show . lineNo $ lno
         lineRef = toValue . ('#':) . show . lineNo $ lno
         wrapElement = if lineAnchors opts
                then H.a
                else H.div

formatHtmlBlockPre :: FormatOptions -> [SourceLine] -> Html
formatHtmlBlockPre opts = H.pre . formatHtmlInline opts

-- | Format tokens as an HTML @pre@ block. Each line is wrapped in a div
-- with the class ‘source-line’. The whole code block is wrapped in a @div@
-- element to aid styling (e.g. the overflow-x property). If line numbering
-- is selected, this surrounding div is given the class ‘number-source’.
-- See the documentation for 'formatHtmlInline' for information about how
-- tokens are encoded.
formatHtmlBlock :: FormatOptions -> [SourceLine] -> Html
formatHtmlBlock opts ls = H.div ! A.class_ sourceCode $
                            pre ! A.class_ (toValue $ Text.unwords classes)
  where  sourceCode = toValue . Text.unwords $ Text.pack "sourceCode" :
                      if numberLines opts
                        then [Text.pack "numberSource"]
                        else []
         classes = Text.pack "sourceCode" :
                   [x | x <- containerClasses opts, x /= Text.pack "sourceCode"]
         pre = formatHtmlBlockPre opts ls

-- | Returns CSS for styling highlighted code according to the given style.
styleToCss :: Style -> String
styleToCss f = unlines $ divspec ++ numberspec ++ colorspec ++ linkspec ++ map toCss (tokenStyles f)
   where colorspec = case (defaultColor f, backgroundColor f) of
                          (Nothing, Nothing) -> []
                          (Just c, Nothing)  -> ["pre, code { color: " ++ fromColor c ++ "; }"]
                          (Nothing, Just c)  -> ["pre, code { background-color: " ++ fromColor c ++ "; }"]
                          (Just c1, Just c2) -> ["pre, code { color: " ++ fromColor c1 ++ "; background-color: " ++
                                                  fromColor c2 ++ "; }"]
         numberspec = [
            ".numberSource div.sourceLine, .numberSource a.sourceLine"
          , "  { position: relative; }"
          , ".numberSource div.sourceLine::before, .numberSource a.sourceLine::before"
          , "  { content: attr(data-line-number);"
          , "    position: absolute; left: -5em; text-align: right; vertical-align: baseline;"
          , "    border: none; pointer-events: all; "
          , "    -webkit-touch-callout: none; -webkit-user-select: none;"
          , "    -khtml-user-select: none; -moz-user-select: none;"
          , "    -ms-user-select: none; user-select: none;"
          , "    padding: 0 4px; width: 4em; }"
          , ".numberSource pre.sourceCode { margin-left: 3em;" ++
              maybe "" (\c -> "border-left: 1px solid " ++ fromColor c ++ "; ") (lineNumberColor f) ++
              maybe "" (\c -> "background-color: " ++ fromColor c ++ "; ") (lineNumberBackgroundColor f) ++
              maybe "" (\c -> "color: " ++ fromColor c ++ "; ") (lineNumberColor f) ++
              " padding-left: 4px; }"
          ]
         divspec = [ "div.sourceCode { overflow-x: auto; }"
          , "div.sourceLine, a.sourceLine { display: inline-block; min-height: 1.25em; }"
          , "a.sourceLine { pointer-events: none; color: inherit; text-decoration: inherit; }"
          , ".sourceCode { overflow: visible; }"
          , "@media print {"
          , "code.sourceCode { white-space: pre-wrap; }"
          , "div.sourceLine, a.sourceLine { text-indent: -1em; padding-left: 1em; }"
          , "}"
          ]
         linkspec = [ "@media screen {"
          , "a.sourceLine::before { text-decoration: underline; color = initial; }"
          , "}"
          ]

toCss :: (TokenType, TokenStyle) -> String
toCss (t,tf) = "code > div > span." ++ short t ++ ", "
                ++ "code > a > span." ++ short t ++ " { "
                ++ colorspec ++ backgroundspec ++ weightspec ++ stylespec
                ++ decorationspec ++ "} /* " ++ showTokenType t ++ " */"
  where colorspec = maybe "" (\col -> "color: " ++ fromColor col ++ "; ") $ tokenColor tf
        backgroundspec = maybe "" (\col -> "background-color: " ++ fromColor col ++ "; ") $ tokenBackground tf
        weightspec = if tokenBold tf then "font-weight: bold; " else ""
        stylespec  = if tokenItalic tf then "font-style: italic; " else ""
        decorationspec = if tokenUnderline tf then "text-decoration: underline; " else ""
        showTokenType t' = case reverse (show t') of
                             'k':'o':'T':xs -> reverse xs
                             _              -> ""

