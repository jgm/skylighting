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
import qualified Data.Map as M
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
       then "number: true, start: " <> Text.pack (show (startNumber opts)) <> ", "
       else "") <>
  "(" <> -- an array
  Text.intercalate "\n" (map (\ln -> "[" <> formatTypstInline opts [ln] <> "],") ls)
  <> "));"

-- | Converts a 'Style' to a set of Typst macro definitions,
-- which should be placed in the document's preamble.
styleToTypst :: Style -> Text
styleToTypst f =
  Text.unlines $
  [ "#let EndLine() = raw(\"\\n\")"
  , "#let Skylighting(fill: none, number: false, start: 1, sourcelines) = {"
  , "   let blocks = []"
  , "   let lnum = start - 1"
  , "   for ln in sourcelines {"
  , "     if number { lnum = lnum + 1; blocks = blocks + box(width: 2em, [ #lnum ]) }"
  , "     blocks = blocks + ln + EndLine()"
  , "   }"
  , "   let bgcolor = " <> case backgroundColor f of
                             Nothing -> "none"
                             Just c -> "rgb(" <>
                                        Text.pack (show (fromColor c :: String)) <>
                                        ")"
  , "   block(fill: bgcolor, blocks)"
  , "}"
  ] <>
  sort (map (macrodef (defaultColor f) (Map.toList (tokenStyles f)))
         (enumFromTo KeywordTok NormalTok))

macrodef :: Maybe Color -> [(TokenType, TokenStyle)] -> TokenType -> Text
macrodef defaultcol tokstyles' tokt =
  "#let " <> Text.pack (show tokt) <> "(s) = " <> (ul . bg . textstyle) ("raw(s)")
 where tokstyles = M.fromList tokstyles'
       tokf = case M.lookup tokt tokstyles of
                    Nothing -> defStyle
                    Just x  -> x
       ul x = if tokenUnderline tokf
                 then "underline(" <> x <> ")"
                 else x
       bg x = case tokenBackground tokf of
                   Nothing        -> x
                   Just _c -> x -- TODO?
       textstyle x = "text(" <> bf x <> it x <> co x <> x <> ")"
       it x = if tokenItalic tokf
                 then "style: \"italic\","
                 else ""
       bf x = if tokenBold tokf
                 then "weight: \"bold\","
                 else ""
       co x = case tokenColor tokf `mplus` defaultcol of
                   Just c -> "fill: rgb(" <>
                     Text.pack (show (fromColor c :: String)) <> "),"
                   Nothing -> ""
