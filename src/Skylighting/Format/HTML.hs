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
import qualified Data.Text.Lazy as TL
import Skylighting.Types
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Clay (Css)
import qualified Clay as C
import qualified Clay.Media as CM
import qualified Clay.Text as CT

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
  H.div ! A.class_ (toValue "sourceCode") $
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
styleToCss = TL.unpack . C.renderWith C.compact [] . styleToCss'

styleToCss' :: Style -> Css
styleToCss' f = do
    divspec
    numberspec
    colorspec
    linkspec
    mapM_ toCss (sort (Map.toList (tokenStyles f)))

   where colorspec = C.div C.# sourceCode C.?
           case (defaultColor f, backgroundColor f) of
             (Nothing, Nothing) -> pure ()
             (Just c, Nothing)  -> do
               C.color $ fromColor c
             (Nothing, Just c)  -> do
               C.backgroundColor $ fromColor c
             (Just c1, Just c2) -> do
               C.color $ fromColor c1
               C.backgroundColor $ fromColor c2

         numberspec = do
          C.pre C.# numberSource C.? do
            C.marginLeft . C.em $ 3
            pure () `maybe` (\c -> C.borderLeft C.solid (C.px 1) $ fromColor c) $ lineNumberColor f

            C.paddingLeft . C.px $ 4

            C.a C.# sourceLine C.? do
              C.position C.relative
              C.empty C.& do
                C.position C.absolute

              C.before C.& do
                C.content . C.attrContent . Text.pack $ "data-line-number"
                C.position C.absolute
                C.left . C.em $ (-5)
                C.textAlign . C.alignSide $ C.sideRight
                C.verticalAlign C.vAlignBaseline
                C.border C.none C.nil C.none
                C.pointerEvents C.allEvents
                C.userSelect C.none
                C.sym2 C.padding C.nil (C.px 4)
                C.width . C.em $ 4
                pure () `maybe` (\c -> C.backgroundColor $ fromColor c) $ lineNumberBackgroundColor f
                pure () `maybe` (\c -> C.color $ fromColor c) $ lineNumberColor f

         divspec = do
          C.a C.# sourceLine C.? do
            C.display C.inlineBlock
            C.lineHeight . C.unitless $ 1.25
            C.pointerEvents C.none
            C.color C.inherit
            C.textDecoration C.inherit

            C.empty C.& do
              -- Correct empty line height
              C.height . C.em $ 1.2
              C.position C.absolute

          C.star C.# sourceCode C.? do
            -- Needed for line numbers to be displayed
            C.overflow C.visible

          -- Collapse neighbours correctly
          C.div C.# sourceCode C.? do
            C.sym2 C.margin (C.em 1) C.nil
          C.pre C.# sourceCode C.? do
            C.sym C.margin C.nil

          C.code C.# sourceCode C.? do
            C.whiteSpace CT.pre
            -- Needed for contents to be position: absolute
            C.position C.relative

          C.query CM.screen [] $ do
            C.div C.# sourceCode C.? do
              C.overflowX C.auto

          C.query CM.print [] $ do
            C.code C.# sourceCode C.? do
              C.whiteSpace C.preWrap
            C.a C.# sourceLine C.? do
              C.textIndent . C.indent . C.em $ (-1)
              C.paddingLeft . C.em $ 1

         linkspec = C.query CM.screen [] $ do
          C.a C.# sourceLine C.# C.before C.? do
            C.textDecoration C.underline

         numberSource :: C.Refinement
         numberSource = fromString ".numberSource"
         sourceCode :: C.Refinement
         sourceCode = fromString ".sourceCode"
         sourceLine :: C.Refinement
         sourceLine = fromString ".sourceLine"

toCss :: (TokenType, TokenStyle) -> Css
toCss (t,tf) = do
        C.code C.** C.span C.# shortR t C.? do
            colorspec
            backgroundspec
            weightspec
            stylespec
            decorationspec
            -- C.comment $ showTokenType t
  where colorspec = pure () `maybe` (\col -> C.color $ fromColor col) $ tokenColor tf
        backgroundspec = pure () `maybe` (\col -> C.backgroundColor $ fromColor col) $ tokenBackground tf
        weightspec = if tokenBold tf then C.fontWeight C.bold else pure ()
        stylespec  = if tokenItalic tf then C.fontStyle C.italic else pure ()
        decorationspec = if tokenUnderline tf then C.textDecoration C.underline else pure ()
        shortR = C.byClass . Text.pack . short
        -- showTokenType t' = case reverse (show t') of
        --                      'k':'o':'T':xs -> reverse xs
        --                      _              -> ""

