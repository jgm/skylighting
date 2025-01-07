{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Skylighting.Format.Typst (
         formatTypstInline
       , formatTypstBlock
       , styleToTypst
       ) where

import Control.Monad (mplus)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Skylighting.Types
import Text.Printf
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

-- | Formats tokens as Typst using custom commands inside
-- @|@ characters. Assumes that @|@ is defined as a short verbatim
-- command by the macros produced by 'styleToTypst'.
-- A @KeywordTok@ is rendered using @\\KeywordTok{..}@, and so on.
formatTypstInline :: FormatOptions -> [SourceLine] -> Text
formatTypstInline _opts = Text.intercalate newline . map sourceLineToTypst

newline :: Text
newline =  "#EndLine()\n"

sourceLineToTypst :: SourceLine -> Text
sourceLineToTypst = mconcat . map tokenToTypst

tokenToTypst :: Token -> Text
tokenToTypst (toktype, txt) =
  "#" <> Text.pack (show toktype) <> "(" <> doubleQuoted txt <> ");"

doubleQuoted :: Text -> Text
doubleQuoted t = "\"" <> escape t <> "\""
 where
  escape = Text.concatMap escapeChar
  escapeChar '\\' = "\\\\"
  escapeChar '"' = "\\\""
  escapeChar c = Text.singleton c

-- Typst

-- | Format tokens as a Typst @Highlighting@ environment inside a
-- Skylighting block that can be styled. @Skylighting@ is
-- defined by the macros produced by 'styleToTypst'.
formatTypstBlock :: FormatOptions -> [SourceLine] -> Text
formatTypstBlock opts ls =
  "#Skylighting(" <>
  (if numberLines opts
       then "number: true, start: " <> Text.pack (show (startNumber opts)) <> ")"
       else ")") <>
  "[" <> formatTypstInline opts ls <> "];"

-- | Converts a 'Style' to a set of Typst macro definitions,
-- which should be placed in the document's preamble.
styleToTypst :: Style -> Text
styleToTypst f = "TODO"


-- #let Skylighting(body, number: false, start: 1) = body
-- #let EndLine() = raw("\n")
-- 
-- #let KeywordTok(s) = raw(s)
-- #let DataTypeTok(s) = raw(s)
-- #let DecValTok(s) = raw(s)
-- #let BaseNTok(s) = raw(s)
-- #let FloatTok(s) = raw(s)
-- #let ConstantTok(s) = raw(s)
-- #let CharTok(s) = raw(s)
-- #let SpecialCharTok(s) = raw(s)
-- #let StringTok(s) = raw(s)
-- #let VerbatimStringTok(s) = raw(s)
-- #let SpecialStringTok(s) = raw(s)
-- #let ImportTok(s) = raw(s)
-- #let CommentTok(s) = raw(s)
-- #let DocumentationTok(s) = raw(s)
-- #let AnnotationTok(s) = raw(s)
-- #let CommentVarTok(s) = raw(s)
-- #let OtherTok(s) = raw(s)
-- #let FunctionTok(s) = raw(s)
-- #let VariableTok(s) = raw(s)
-- #let ControlFlowTok(s) = raw(s)
-- #let OperatorTok(s) = raw(s)
-- #let BuiltInTok(s) = raw(s)
-- #let ExtensionTok(s) = raw(s)
-- #let PreprocessorTok(s) = raw(s)
-- #let AttributeTok(s) = raw(s)
-- #let RegionMarkerTok(s) = raw(s)
-- #let InformationTok(s) = raw(s)
-- #let WarningTok(s) = raw(s)
-- #let AlertTok(s) = raw(s)
-- #let ErrorTok(s) = raw(s)
-- #let NormalTok(s) = raw(s)


  -- define Skylighting + all the token functions
--  (case backgroundColor f of
--        Nothing          -> ["\\newenvironment{Shaded}{}{}"]
--        Just (RGB r g b) -> ["\\usepackage{framed}"
--                            ,Text.pack
--                              (printf "\\definecolor{shadecolor}{RGB}{%d,%d,%d}" r g b)
--                            ,"\\newenvironment{Shaded}{\\begin{snugshade}}{\\end{snugshade}}"])
--  ++ sort (map (macrodef (defaultColor f) (Map.toList (tokenStyles f)))
--            (enumFromTo KeywordTok NormalTok))

-- also define EndLine
-- #raw(\"\\n\")"

macrodef :: Maybe Color -> [(TokenType, TokenStyle)] -> TokenType -> Text
macrodef defaultcol tokstyles tokt = "TODO"
-- "\\newcommand{\\"
--   <> Text.pack (show tokt)
--   <> "}[1]{"
--   <> Text.pack (co . ul . bf . it . bg $ "#1")
--   <> "}"
--   where tokf = case lookup tokt tokstyles of
--                      Nothing -> defStyle
--                      Just x  -> x
--         ul x = if tokenUnderline tokf
--                   then "\\underline{" <> x <> "}"
--                   else x
--         it x = if tokenItalic tokf
--                   then "\\textit{" <> x <> "}"
--                   else x
--         bf x = if tokenBold tokf
--                   then "\\textbf{" <> x <> "}"
--                   else x
--         bcol = fromColor `fmap` tokenBackground tokf
--                   :: Maybe (Double, Double, Double)
--         bg x = case bcol of
--                     Nothing        -> x
--                     Just (r, g, b) ->
--                        printf "\\colorbox[rgb]{%0.2f,%0.2f,%0.2f}{%s}" r g b x
--         col  = fromColor `fmap` (tokenColor tokf `mplus` defaultcol)
--                   :: Maybe (Double, Double, Double)
--         co x = case col of
--                     Nothing        -> x
--                     Just (r, g, b) ->
--                         printf "\\textcolor[rgb]{%0.2f,%0.2f,%0.2f}{%s}" r g b x

