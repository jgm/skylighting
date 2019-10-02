{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Skylighting.Format.ANSI (
         formatANSI
       ) where
import Control.Monad (mplus)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Skylighting.Types
import qualified System.Console.ANSI.Codes as ANSI
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

formatANSI :: FormatOptions -> Style -> [SourceLine] -> Text
formatANSI opts sty = (beforeText <>)
                        . (<> afterText)
                        . Text.intercalate (Text.singleton '\n')
                        . zipWith (sourceLineToANSI opts sty) [startNum..]
    where beforeText = ansiResetText <> ansiStyleText clv (defaultColor sty) (backgroundColor sty) False False False
          afterText = ansiResetText
          startNum = LineNo $ startNumber opts
          clv = ansiColorLevel opts

sourceLineToANSI :: FormatOptions -> Style -> LineNo -> SourceLine -> Text
sourceLineToANSI opts sty lno = prependLineNoText
                                 . mconcat
                                 . map (tokenToANSI clv sty)
    where prependLineNoText = if numberLines opts
                                 then (lineNoText <>)
                                 else id
          lineNoText = ansiStyleText clv lineNoFgc lineNoBgc False False False
                         <> Text.pack (show $ lineNo lno)
                         <> ansiStyleText clv (defaultColor sty) (backgroundColor sty) False False False
                         <> "\t"
          lineNoFgc = lineNumberColor sty `mplus` defaultColor sty
          lineNoBgc = lineNumberBackgroundColor sty `mplus` backgroundColor sty
          clv = ansiColorLevel opts

tokenToANSI :: ANSIColorLevel -> Style -> Token -> Text
tokenToANSI clv sty (tokTy, tokText) = ansiStyleText clv tokFgc tokBgc tokB tokI tokU
                                         <> tokText
                                         <> ansiStyleText clv (defaultColor sty) (backgroundColor sty) False False False
    where TokenStyle tokFgcRaw tokBgcRaw tokB tokI tokU = fromMaybe defStyle . Map.lookup tokTy $ tokenStyles sty
          tokFgc = tokFgcRaw `mplus` defaultColor sty
          tokBgc = tokBgcRaw `mplus` backgroundColor sty

ansiStyleText :: ANSIColorLevel -- ^ color support level
            -> Maybe Color -- ^ foreground
            -> Maybe Color -- ^ background
            -> Bool -- ^ bold
            -> Bool -- ^ italic
            -> Bool -- ^ underlined
            -> Text
ansiStyleText clv fgc bgc b i u = optReset <> sgrTextFg <> sgrTextBg
                                    <> (Text.pack . ANSI.setSGRCode $ concat [sgrCodeFg,
                                                                              sgrCodeBg,
                                                                              sgrCodeBold,
                                                                              sgrCodeItal,
                                                                              sgrCodeUndl])
    -- FIXME: the @ansi-terminal@ library should do the 256-color parts more cleanly someday
    where (sgrCodeFg, sgrTextFg) = case clv of
            ANSITrueColor -> (maybeToList $ fmap (ANSI.SetRGBColor ANSI.Foreground . fromColor) fgc, "")
            ANSI256Color -> ([], fromMaybe "" $ fmap (\c -> Text.pack $ ANSI.csi [38, 5,
              fromIntegral . getXterm256ColorCode $ fromColor c] "m") fgc)
            ANSI16Color -> (maybeToList $ fmap (uncurry (ANSI.SetColor ANSI.Foreground) . fromColor) fgc, "")
          (sgrCodeBg, sgrTextBg) = case clv of
            ANSITrueColor -> (maybeToList $ fmap (ANSI.SetRGBColor ANSI.Background . fromColor) bgc, "")
            ANSI256Color -> ([], fromMaybe "" $ fmap (\c -> Text.pack $ ANSI.csi [48, 5,
              fromIntegral . getXterm256ColorCode $ fromColor c] "m") bgc)
            ANSI16Color -> (maybeToList $ fmap (uncurry (ANSI.SetColor ANSI.Background) . fromColor) bgc, "")
          optReset = if isNothing fgc && isNothing bgc then ansiResetText else ""
          sgrCodeBold = [ANSI.SetConsoleIntensity $ if b then ANSI.BoldIntensity else ANSI.NormalIntensity]
          sgrCodeItal = [ANSI.SetItalicized i] -- FIXME: Not very widely supported in terminals
          sgrCodeUndl = [ANSI.SetUnderlining $ if u then ANSI.SingleUnderline else ANSI.NoUnderline]

ansiResetText :: Text
ansiResetText = Text.pack $ ANSI.setSGRCode [ANSI.Reset]
