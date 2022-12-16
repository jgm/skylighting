{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Skylighting.Format.ConTeXt
  ( formatConTeXtInline
  , formatConTeXtBlock
  , styleToConTeXt
  ) where

import Control.Monad (mplus)
import Data.Char (isSpace)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Skylighting.Types
import Text.Printf
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

formatConTeXt :: [SourceLine] -> Text
formatConTeXt = Text.intercalate (Text.singleton '\n')
                       . map sourceLineToConTeXt

-- | Formats tokens as ConTeXt using custom commands inside a @\type{}@.
-- A @KeywordTok@ is rendered using @\\KeywordTok{..}@, and so on.
formatConTeXtInline :: FormatOptions -> [SourceLine] -> Text
formatConTeXtInline _opts ls =
  "\\highlight{" <> formatConTeXt ls <> "}"

sourceLineToConTeXt :: SourceLine -> Text
sourceLineToConTeXt =
  Text.replace "/ETEX/BTEX" "" .
  Text.replace "/ETEX /BTEX" " " .
  mconcat . map tokenToConTeXt

tokenToConTeXt :: Token -> Text
tokenToConTeXt (NormalTok, txt)
  | Text.all isSpace txt = escapeConTeXt txt
tokenToConTeXt (toktype, txt)   = "/BTEX\\" <>
  (Text.pack (show toktype) <> "{" <> escapeConTeXt txt <> "}/ETEX")

escapeConTeXt :: Text -> Text
escapeConTeXt = Text.concatMap escapeConTeXtChar
  where escapeConTeXtChar c =
         case c of
           '\\' -> "\\letterbackslash "
           '{'  -> "\\letteropenbrace "
           '}'  -> "\\letterclosebrace "
           '|'  -> "\\letterbar "
           '$'  -> "\\letterdollar "
           '_'  -> "\\letterunderscore "
           '%'  -> "\\letterpercent "
           '#'  -> "\\letterhash "
           '/'  -> "\\letterslash "
           '~'  -> "\\lettertilde "
           _    -> Text.singleton c

-- ConTeXt

-- | Format tokens as a ConTeXt @highlighting@ typing environment. The
-- @highlighting@ environemnt is defined by the macros produced by
-- 'styleToConTeXt'; it is a @typing@ environment with default escaping
-- enabled, i.e., @/@ is the escape character.
formatConTeXtBlock :: FormatOptions -> [SourceLine] -> Text
formatConTeXtBlock opts ls = Text.unlines
  [ "\\starthighlighting" <>
    (if numberLines opts
     then "[numbering=line]"
     else Text.empty)
  , formatConTeXt ls
  , "\\stophighlighting"
  ]

-- | Converts a 'Style' to a set of ConTeXt command definitions,
-- which should be placed in the document's preamble.
styleToConTeXt :: Style -> Text
styleToConTeXt f = Text.unlines $
  ( case backgroundColor f of
         Nothing          -> id
         Just (RGB r g b) -> (:)
           (Text.pack $ printf "\\definecolor[shadecolor][x=%x%x%x]" r g b)
  ) $
  [ "\\defineframedtext [shaded]"
  , "  [backgroundcolor=shadecolor,"
  , "   background=color,"
  , "   frame=off,"
  , "   offset=0pt,"
  , "   width=local]"
  , "\\definetyping [highlighting]"
  , "  [escape=yes,"
  , "   before={\\startshaded},"
  , "   after={\\stopshaded}]"
  , "\\definetype [highlight]"
  , "  [escape=yes]"
  ] ++

  sort (map (macrodef (defaultColor f) (Map.toList (tokenStyles f)))
            (enumFromTo KeywordTok NormalTok))

macrodef :: Maybe Color -> [(TokenType, TokenStyle)] -> TokenType -> Text
macrodef defaultcol tokstyles tokt = "\\define[1]\\"
  <> Text.pack (show tokt)
  <> "{"
  <> Text.pack (co . ul . bf . it $ "#1")
  <> "}"
  where tokf = fromMaybe defStyle $ lookup tokt tokstyles
        ul x = if tokenUnderline tokf
                  then "\\underbar{" <> x <> "}"
                  else x
        it x = if tokenItalic tokf
                  then "\\em " <> x
                  else x
        bf x = if tokenBold tokf
                  then "\\bf " <> x
                  else x
        col  = fromColor `fmap` (tokenColor tokf `mplus` defaultcol)
                 :: Maybe (Double, Double, Double)
        co x = case col of
                 Nothing        -> x
                 Just (r, g, b) ->
                   printf "\\colored[r=%0.2f,g=%0.2f,b=%0.2f]{%s}" r g b x

