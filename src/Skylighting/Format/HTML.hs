module Skylighting.Format.HTML (
      formatHtmlInline
    , formatHtmlBlock
    , styleToCss
    ) where

import Data.List (intersperse, sort)
import qualified Data.Map as Map
import Data.Monoid ((<>))
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
formatHtmlInline opts = wrapCode opts
                      . mconcat . intersperse (toHtml "\n")
                      . map (mapM_ (tokenToHtml opts))

-- | Format tokens as an HTML @pre@ block. Each line is wrapped in an a
-- element with the class ‘source-line’. If line numbering
-- is selected, the surrounding pre is given the class ‘numberSource’,
-- and the resulting html will display line numbers thanks to the included
-- CSS.  See the documentation for 'formatHtmlInline' for information about how
-- tokens are encoded.
formatHtmlBlock :: FormatOptions -> [SourceLine] -> Html
formatHtmlBlock opts ls =
  H.pre ! A.class_ (toValue $ Text.unwords classes)
        $ wrapCode opts
        $ mconcat . intersperse (toHtml "\n")
        $ zipWith (sourceLineToHtml opts) [startNum..] ls
  where  classes = Text.pack "sourceCode" :
                   [Text.pack "numberSource" | numberLines opts] ++
                   [x | x <- containerClasses opts
                      , x /= Text.pack "sourceCode"]
         startNum = LineNo $ startNumber opts

wrapCode :: FormatOptions -> Html -> Html
wrapCode opts h = H.code ! A.class_ (toValue $ Text.unwords
                                             $ Text.pack "sourceCode"
                                               : codeClasses opts) $ h

-- | Each line of source is wrapped in an (inline-block) anchor that makes
-- subsequent per-line processing (e.g. adding line numnbers) possible.
sourceLineToHtml :: FormatOptions -> LineNo -> SourceLine -> Html
sourceLineToHtml opts lno cont =
  (if lineAnchors opts
      then H.a   ! A.class_ sourceLine
                 ! A.id lineNum
                 ! A.href lineRef
                 ! dataAttrib
      else H.a   ! A.class_ sourceLine
                 ! A.id lineNum
                 ! dataAttrib) $ mapM_ (tokenToHtml opts) cont
  where  sourceLine = toValue "sourceLine"
         lineNum = toValue prefixedLineNo
         lineRef = toValue ('#':prefixedLineNo)
         prefixedLineNo = Text.unpack (lineIdPrefix opts) <> show (lineNo lno)
         dataAttrib = H.dataAttribute (fromString "line-number")
                          (toValue (show (lineNo lno)))

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

-- | Returns CSS for styling highlighted code according to the given style.
styleToCss :: Style -> String
styleToCss f = unlines $
  divspec ++ numberspec ++ colorspec ++ linkspec ++
    sort (map toCss (Map.toList (tokenStyles f)))
   where colorspec = case (defaultColor f, backgroundColor f) of
                          (Nothing, Nothing) -> []
                          (Just c, Nothing)  -> ["pre, code { color: " ++ fromColor c ++ "; }"]
                          (Nothing, Just c)  -> ["pre, code { background-color: " ++ fromColor c ++ "; }"]
                          (Just c1, Just c2) -> ["pre, code { color: " ++ fromColor c1 ++ "; background-color: " ++
                                                  fromColor c2 ++ "; }"]
         numberspec = [
            "pre.numberSource a.sourceLine"
          , "  { position: relative; }"
          , "pre.numberSource a.sourceLine::before"
          , "  { content: attr(data-line-number);"
          , "    position: absolute; left: -5em; text-align: right; vertical-align: baseline;"
          , "    border: none; pointer-events: all;"
          , "    -webkit-touch-callout: none; -webkit-user-select: none;"
          , "    -khtml-user-select: none; -moz-user-select: none;"
          , "    -ms-user-select: none; user-select: none;"
          , "    padding: 0 4px; width: 4em;"
          , maybe "" (\c -> "    background-color: " ++ fromColor c ++ ";\n")
              (lineNumberBackgroundColor f) ++
            maybe "" (\c -> "    color: " ++ fromColor c ++ ";\n")
              (lineNumberColor f) ++
            "  }"
          , "pre.numberSource { margin-left: 3em; " ++
              maybe "" (\c -> "border-left: 1px solid " ++ fromColor c ++ "; ") (lineNumberColor f) ++
              " padding-left: 4px; }"
          ]
         divspec = [
            "a.sourceLine { display: inline-block; min-height: 1.25em; }"
          , "a.sourceLine { pointer-events: none; color: inherit; text-decoration: inherit; }"
          , ".sourceCode { overflow: visible; }"
          , "code.sourceCode { white-space: pre; }"
          , "@media print {"
          , "code.sourceCode { white-space: pre-wrap; }"
          , "a.sourceLine { text-indent: -1em; padding-left: 1em; }"
          , "}"
          ]
         linkspec = [ "@media screen {"
          , "a.sourceLine::before { text-decoration: underline; color: initial; }"
          , "}"
          ]

toCss :: (TokenType, TokenStyle) -> String
toCss (t,tf) = "code span." ++ short t ++ " { "
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

