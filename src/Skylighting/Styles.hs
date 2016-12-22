module Skylighting.Styles (
    parseTheme
  , pygments
  , kate
  , espresso
  , tango
  , haddock
  , monochrome
  , zenburn) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Skylighting.Types

-- | Parse a KDE theme JSON document into a skylighting Style.
parseTheme :: ByteString -> Either String Style
parseTheme = eitherDecode

color :: Int -> Maybe Color
color = toColor

-- | Style based on pygments's default colors.
pygments :: Style
pygments = Style{
    backgroundColor = Nothing
  , defaultColor = Nothing
  , lineNumberColor = color 0xaaaaaa
  , lineNumberBackgroundColor = Nothing
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenColor = color 0x007020, tokenBold = True })
    , (DataTypeTok, defStyle{ tokenColor = color 0x902000 })
    , (DecValTok, defStyle{ tokenColor = color 0x40a070 })
    , (BaseNTok, defStyle{ tokenColor = color 0x40a070 })
    , (FloatTok, defStyle{ tokenColor = color 0x40a070 })
    , (CharTok, defStyle{ tokenColor = color 0x4070a0 })
    , (StringTok, defStyle{ tokenColor = color 0x4070a0 })
    , (CommentTok, defStyle{ tokenColor = color 0x60a0b0, tokenItalic = True })
    , (OtherTok, defStyle{ tokenColor = color 0x007020 })
    , (AlertTok, defStyle{ tokenColor = color 0xff0000, tokenBold = True })
    , (FunctionTok, defStyle{ tokenColor = color 0x06287e })
    , (ErrorTok, defStyle{ tokenColor = color 0xff0000, tokenBold = True })
    , (WarningTok, defStyle{ tokenColor = color 0x60a0b0, tokenItalic = True, tokenBold = True })
    , (ConstantTok, defStyle{ tokenColor = color 0x880000 })
    , (SpecialCharTok, defStyle{ tokenColor = color 0x4070a0 })
    , (VerbatimStringTok, defStyle{ tokenColor = color 0x4070a0 })
    , (SpecialStringTok, defStyle{ tokenColor = color 0xBB6688 })
    , (ImportTok, defStyle)
    , (VariableTok, defStyle{ tokenColor = color 0x19177C })
    , (ControlFlowTok, defStyle{ tokenColor = color 0x007020, tokenBold = True })
    , (OperatorTok, defStyle{ tokenColor = color 0x666666 })
    , (BuiltInTok, defStyle)
    , (ExtensionTok, defStyle)
    , (PreprocessorTok, defStyle{ tokenColor = color 0xBC7A00 })
    , (AttributeTok, defStyle{ tokenColor = color 0x7D9029 })
    , (DocumentationTok, defStyle{ tokenColor = color 0xBA2121, tokenItalic = True })
    , (AnnotationTok, defStyle{ tokenColor = color 0x60a0b0, tokenItalic = True, tokenBold = True })
    , (CommentVarTok, defStyle{ tokenColor = color 0x60a0b0, tokenItalic = True, tokenBold = True })
    , (InformationTok, defStyle{ tokenColor = color 0x60a0b0, tokenItalic = True, tokenBold = True })
    ]
  }

-- | Style based on kate's default colors.
kate :: Style
kate = Style{
      tokenStyles =
        [ ( KeywordTok, defStyle { tokenColor = Just (RGB 31 28 27), tokenBold = True })
        , ( DataTypeTok, defStyle { tokenColor = Just (RGB 0 87 174) })
        , ( DecValTok, defStyle { tokenColor = Just (RGB 176 128 0) })
        , ( BaseNTok, defStyle { tokenColor = Just (RGB 176 128 0) })
        , ( FloatTok, defStyle { tokenColor = Just (RGB 176 128 0) })
        , ( ConstantTok, defStyle { tokenColor = Just (RGB 170 85 0) })
        , ( CharTok, defStyle { tokenColor = Just (RGB 146 76 157) })
        , ( SpecialCharTok, defStyle { tokenColor = Just (RGB 61 174 233) })
        , ( StringTok, defStyle { tokenColor = Just (RGB 191 3 3) })
        , ( VerbatimStringTok, defStyle { tokenColor = Just (RGB 191 3 3) })
        , ( SpecialStringTok, defStyle { tokenColor = Just (RGB 255 85 0) })
        , ( ImportTok, defStyle { tokenColor = Just (RGB 255 85 0) })
        , ( CommentTok, defStyle { tokenColor = Just (RGB 137 136 135) })
        , ( DocumentationTok, defStyle { tokenColor = Just (RGB 96 120 128) })
        , ( AnnotationTok, defStyle { tokenColor = Just (RGB 202 96 202) })
        , ( CommentVarTok, defStyle { tokenColor = Just (RGB 0 149 255) })
        , ( OtherTok, defStyle { tokenColor = Just (RGB 0 110 40) })
        , ( FunctionTok, defStyle { tokenColor = Just (RGB 100 74 155) })
        , ( VariableTok, defStyle { tokenColor = Just (RGB 0 87 174) })
        , ( ControlFlowTok, defStyle { tokenColor = Just (RGB 31 28 27), tokenBold = True })
        , ( OperatorTok, defStyle { tokenColor = Just (RGB 31 28 27) })
        , ( BuiltInTok, defStyle { tokenColor = Just (RGB 100 74 155), tokenBold = True })
        , ( ExtensionTok, defStyle { tokenColor = Just (RGB 0 149 255), tokenBold = True })
        , ( PreprocessorTok, defStyle { tokenColor = Just (RGB 0 110 40) })
        , ( AttributeTok, defStyle { tokenColor = Just (RGB 0 87 174) })
        , ( RegionMarkerTok, defStyle { tokenColor = Just (RGB 0 87 174) })
        , ( InformationTok, defStyle { tokenColor = Just (RGB 176 128 0) })
        , ( WarningTok, defStyle { tokenColor = Just (RGB 191 3 3) })
        , ( AlertTok, defStyle { tokenColor = Just (RGB 191 3 3), tokenBold = True })
        , ( ErrorTok, defStyle { tokenColor = Just (RGB 191 3 3), tokenUnderline = True })
        , ( NormalTok, defStyle { tokenColor = Just (RGB 31 28 27) })
        ]
    , defaultColor = Just (RGB 31 28 27)
    , backgroundColor = Just (RGB 255 255 255)
    , lineNumberColor = Just (RGB 160 160 160)
    , lineNumberBackgroundColor = Just (RGB 255 255 255)
    }

-- | Style based on pygments's tango colors.
tango :: Style
tango = Style{
    backgroundColor = color 0xf8f8f8
  , defaultColor = Nothing
  , lineNumberColor = color 0xaaaaaa
  , lineNumberBackgroundColor = Nothing
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenColor = color 0x204a87, tokenBold = True })
    , (DataTypeTok, defStyle{ tokenColor = color 0x204a87 })
    , (DecValTok, defStyle{ tokenColor = color 0x0000cf })
    , (BaseNTok, defStyle{ tokenColor = color 0x0000cf })
    , (FloatTok, defStyle{ tokenColor = color 0x0000cf })
    , (CharTok, defStyle{ tokenColor = color 0x4e9a06 })
    , (StringTok, defStyle{ tokenColor = color 0x4e9a06 })
    , (CommentTok, defStyle{ tokenColor = color 0x8f5902, tokenItalic = True })
    , (OtherTok, defStyle{ tokenColor = color 0x8f5902 })
    , (AlertTok, defStyle{ tokenColor = color 0xef2929 })
    , (FunctionTok, defStyle{ tokenColor = color 0x000000 })
    , (ErrorTok, defStyle{ tokenColor = color 0xa40000, tokenBold = True })
    , (WarningTok, defStyle{ tokenColor = color 0x8f5902, tokenItalic = True,tokenBold = True })
    , (ConstantTok, defStyle{ tokenColor = color 0x000000 })
    , (SpecialCharTok, defStyle{ tokenColor = color 0x000000 })
    , (VerbatimStringTok, defStyle{ tokenColor = color 0x4e9a06 })
    , (SpecialStringTok, defStyle{ tokenColor = color 0x4e9a06 })
    , (ImportTok, defStyle)
    , (VariableTok, defStyle{ tokenColor = color 0x000000 })
    , (ControlFlowTok, defStyle{ tokenColor = color 0x204a87, tokenBold = True })
    , (OperatorTok, defStyle{ tokenColor = color 0xce5c00, tokenBold = True })
    , (PreprocessorTok, defStyle{ tokenColor = color 0x8f5902, tokenItalic = True} )
    , (ExtensionTok, defStyle)
    , (AttributeTok, defStyle{ tokenColor = color 0xc4a000 })
    , (DocumentationTok, defStyle{ tokenColor = color 0x8f5902, tokenItalic = True, tokenBold = True })
    , (AnnotationTok, defStyle{ tokenColor = color 0x8f5902, tokenItalic = True,tokenBold = True })
    , (CommentVarTok, defStyle{ tokenColor = color 0x8f5902, tokenItalic = True,tokenBold = True })
    , (InformationTok, defStyle{ tokenColor = color 0x8f5902, tokenItalic = True,tokenBold = True })
    ]
  }

-- | Style based on ultraviolet's espresso_libre.css (dark background).
espresso :: Style
espresso = Style{
    backgroundColor = color 0x2A211C
  , defaultColor = color 0xBDAE9D
  , lineNumberColor = color 0xBDAE9D
  , lineNumberBackgroundColor = color 0x2A211C
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenColor = color 0x43A8ED, tokenBold = True })
    , (DataTypeTok, defStyle{ tokenUnderline = True })
    , (DecValTok, defStyle{ tokenColor = color 0x44AA43 })
    , (BaseNTok, defStyle{ tokenColor = color 0x44AA43 })
    , (FloatTok, defStyle{ tokenColor = color 0x44AA43 })
    , (CharTok, defStyle{ tokenColor = color 0x049B0A })
    , (StringTok, defStyle{ tokenColor = color 0x049B0A })
    , (CommentTok, defStyle{ tokenColor = color 0x0066FF, tokenItalic = True })
    , (AlertTok, defStyle{ tokenColor = color 0xffff00 })
    , (FunctionTok, defStyle{ tokenColor = color 0xFF9358, tokenBold = True })
    , (ErrorTok, defStyle{ tokenColor = color 0xffff00, tokenBold = True })
    , (WarningTok, defStyle{ tokenColor = color 0xffff00, tokenBold = True })
    , (ConstantTok, defStyle)
    , (SpecialCharTok, defStyle{ tokenColor = color 0x049B0A })
    , (VerbatimStringTok, defStyle{ tokenColor = color 0x049B0A })
    , (SpecialStringTok, defStyle{ tokenColor = color 0x049B0A })
    , (ImportTok, defStyle)
    , (VariableTok, defStyle)
    , (ControlFlowTok, defStyle{ tokenColor = color 0x43A8ED, tokenBold = True })
    , (OperatorTok, defStyle)
    , (BuiltInTok, defStyle)
    , (ExtensionTok, defStyle)
    , (PreprocessorTok, defStyle{ tokenBold = True })
    , (AttributeTok, defStyle)
    , (DocumentationTok, defStyle{ tokenColor = color 0x0066FF, tokenItalic = True })
    , (AnnotationTok, defStyle{ tokenColor = color 0x0066FF, tokenItalic = True, tokenBold = True })
    , (CommentTok, defStyle{ tokenColor = color 0x0066FF, tokenItalic = True, tokenBold = True })
    , (InformationTok, defStyle{ tokenColor = color 0x0066FF, tokenItalic = True, tokenBold = True })
    ]
  }

-- | Style based on haddock's source highlighting.
haddock :: Style
haddock = Style{
    backgroundColor = Nothing
  , defaultColor = Nothing
  , lineNumberColor = color 0xaaaaaa
  , lineNumberBackgroundColor = Nothing
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenColor = color 0x0000FF })
    , (CharTok, defStyle{ tokenColor = color 0x008080 })
    , (StringTok, defStyle{ tokenColor = color 0x008080 })
    , (CommentTok, defStyle{ tokenColor = color 0x008000 })
    , (OtherTok, defStyle{ tokenColor = color 0xff4000 })
    , (AlertTok, defStyle{ tokenColor = color 0xff0000 })
    , (ErrorTok, defStyle{ tokenColor = color 0xff0000, tokenBold = True })
    , (WarningTok, defStyle{ tokenColor = color 0x008000, tokenBold = True })
    , (ConstantTok, defStyle)
    , (SpecialCharTok, defStyle{ tokenColor = color 0x008080 })
    , (VerbatimStringTok, defStyle{ tokenColor = color 0x008080 })
    , (SpecialStringTok, defStyle{ tokenColor = color 0x008080 })
    , (ImportTok, defStyle)
    , (VariableTok, defStyle)
    , (ControlFlowTok, defStyle{ tokenColor = color 0x0000FF })
    , (OperatorTok, defStyle)
    , (BuiltInTok, defStyle)
    , (ExtensionTok, defStyle)
    , (PreprocessorTok, defStyle{ tokenColor = color 0xff4000 })
    , (DocumentationTok, defStyle{ tokenColor = color 0x008000 })
    , (AnnotationTok, defStyle{ tokenColor = color 0x008000 })
    , (CommentVarTok, defStyle{ tokenColor = color 0x008000 })
    , (AttributeTok, defStyle)
    , (InformationTok, defStyle{ tokenColor = color 0x008000 })
    ]
  }

-- | Style with no colors.
monochrome :: Style
monochrome = Style{
    backgroundColor = Nothing
  , defaultColor = Nothing
  , lineNumberColor = Nothing
  , lineNumberBackgroundColor = Nothing
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenBold = True })
    , (DataTypeTok, defStyle{ tokenUnderline = True })
    , (CommentTok, defStyle{ tokenItalic = True })
    , (AlertTok, defStyle{ tokenBold = True })
    , (ErrorTok, defStyle{ tokenBold = True })
    , (WarningTok, defStyle{ tokenItalic = True })
    , (ControlFlowTok, defStyle{ tokenBold = True })
    , (PreprocessorTok, defStyle{ tokenBold = True })
    , (DocumentationTok, defStyle{ tokenItalic = True })
    , (AnnotationTok, defStyle{ tokenItalic = True })
    , (CommentVarTok, defStyle{ tokenItalic = True })
    , (InformationTok, defStyle{ tokenItalic = True })
    ]
  }

-- | Style based on the popular zenburn vim color scheme
zenburn :: Style
zenburn = Style{
    backgroundColor = color 0x303030
  , defaultColor = color 0xcccccc
  , lineNumberColor = Nothing
  , lineNumberBackgroundColor = Nothing
  , tokenStyles =
    [ (KeywordTok, defStyle{ tokenColor = color 0xf0dfaf })
    , (DataTypeTok, defStyle{ tokenColor = color 0xdfdfbf })
    , (DecValTok, defStyle{ tokenColor = color 0xdcdccc })
    , (BaseNTok, defStyle{ tokenColor = color 0xdca3a3 })
    , (FloatTok, defStyle{ tokenColor = color 0xc0bed1 })
    , (CharTok, defStyle{ tokenColor = color 0xdca3a3 })
    , (StringTok, defStyle{ tokenColor = color 0xcc9393 })
    , (CommentTok, defStyle{ tokenColor = color 0x7f9f7f })
    , (OtherTok, defStyle{ tokenColor = color 0xefef8f })
    , (AlertTok, defStyle{ tokenColor = color 0xffcfaf })
    , (FunctionTok, defStyle{ tokenColor = color 0xefef8f })
    , (ErrorTok, defStyle{ tokenColor = color 0xc3bf9f })
    , (WarningTok, defStyle{ tokenColor = color 0x7f9f7f, tokenBold = True })
    , (ConstantTok, defStyle{ tokenColor = color 0xdca3a3, tokenBold = True })
    , (SpecialCharTok, defStyle{ tokenColor = color 0xdca3a3 })
    , (VerbatimStringTok, defStyle{ tokenColor = color 0xcc9393 })
    , (SpecialStringTok, defStyle{ tokenColor = color 0xcc9393 })
    , (ImportTok, defStyle)
    , (VariableTok, defStyle)
    , (ControlFlowTok, defStyle{ tokenColor = color 0xf0dfaf })
    , (OperatorTok, defStyle{ tokenColor = color 0xf0efd0 })
    , (BuiltInTok, defStyle)
    , (ExtensionTok, defStyle)
    , (PreprocessorTok, defStyle{ tokenColor = color 0xffcfaf, tokenBold = True })
    , (AttributeTok, defStyle)
    , (DocumentationTok, defStyle{ tokenColor = color 0x7f9f7f })
    , (AnnotationTok, defStyle{ tokenColor = color 0x7f9f7f, tokenBold = True })
    , (CommentVarTok, defStyle{ tokenColor = color 0x7f9f7f, tokenBold = True })
    , (InformationTok, defStyle{ tokenColor = color 0x7f9f7f, tokenBold = True })
    ]
  }
