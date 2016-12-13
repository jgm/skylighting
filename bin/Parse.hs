{-# LANGUAGE Arrows #-}

import Safe
import qualified Data.Text as Text
import Text.XML.HXT.Core
import Data.List ((\\))
import Control.Monad
import Highlighting.Kate.Parser
import System.Environment (getArgs)

data SyntaxDefinition =
  SyntaxDefinition { synLanguage      :: String
                   , synAuthor        :: String
                   , synVersion       :: String
                   , synLicense       :: String
                   , synExtensions    :: String
                   , synCaseSensitive :: Bool
                   , synLists         :: [(String, [String])]
                   , synContexts      :: [SyntaxContext]
                   , synItemDatas     :: [(String, String)]
                   , synKeywordAttr   :: SyntaxKeywordAttr
                   } deriving (Show)

data SyntaxKeywordAttr =
  SyntaxKeywordAttr  { keywordCaseSensitive   :: Bool
                     , keywordDelims          :: [Char]
                     } deriving (Show)

data SyntaxContext =
  SyntaxContext { contName               :: String
                , contAttribute          :: String
                , contLineEndContext     :: String
                , contLineBeginContext   :: String
                , contFallthrough        :: Bool
                , contFallthroughContext :: String
                , contDynamic            :: Bool
                , contParsers            :: [Rule]
                } deriving (Show)

{-
data SyntaxParser =
  SyntaxParser { parserType              :: String
               , parserAttribute         :: String
               , parserContext           :: String
               , parserLookAhead         :: Bool
               , parserIncludeAttrib     :: Bool
               , parserFirstNonSpace     :: Bool
               , parserColumn            :: Maybe Int
               , parserDynamic           :: Bool
               , parserString            :: String -- could be a regex
               , parserChar              :: Char
               , parserChar1             :: Char
               , parserChildren          :: [SyntaxParser]
               } deriving (Show)
-}

standardDelims :: [Char]
standardDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\"

defaultKeywordAttr :: SyntaxKeywordAttr
defaultKeywordAttr = SyntaxKeywordAttr { keywordCaseSensitive = True
                                       , keywordDelims = standardDelims }

stripWhitespace :: String -> String
stripWhitespace = reverse . stripWhitespaceLeft . reverse . stripWhitespaceLeft
  where stripWhitespaceLeft = dropWhile isWhitespace
        isWhitespace x = x `elem` [' ', '\t', '\n']

vBool :: Bool -> String -> Bool
vBool defaultVal value = case value of
                           z | z `elem` ["true","yes","1"] -> True
                           z | z `elem` ["false","no","0"] -> False
                           _ -> defaultVal


main = do
  syntaxes <- getArgs >>= mapM (runX . application)
  print syntaxes

application :: String -> IOSArrow b SyntaxDefinition
application src
    = readDocument [withValidate no, withInputEncoding utf8] src
      >>>
      multi (hasName "language")
      >>>
      extractSyntaxDefinition

extractSyntaxDefinition :: IOSArrow XmlTree SyntaxDefinition
extractSyntaxDefinition =  proc x -> do
                             lang <- getAttrValue "name" -< x
                             author <- getAttrValue "author" -< x
                             version <- getAttrValue "version" -< x
                             license <- getAttrValue "license" -< x
                             sources <- getAttrValue "extensions" -< x
                             caseSensitive <- getAttrValue "casesensitive" -< x
                             itemdatas <- getItemDatas -< x
                             lists <- getLists -< x
                             contexts <- getContexts -< x
                             keywordAttr <- getKeywordAttrs -< x
                             returnA -< SyntaxDefinition { synLanguage      = lang
                                                         , synAuthor        = author
                                                         , synVersion       = version
                                                         , synLicense       = license
                                                         , synExtensions    = sources
                                                         , synCaseSensitive = vBool True caseSensitive
                                                         , synLists         = lists
                                                         , synContexts      = contexts
                                                         , synItemDatas     = itemdatas
                                                         , synKeywordAttr   = if null keywordAttr
                                                                                 then defaultKeywordAttr
                                                                                 else head keywordAttr }

getItemDatas :: IOSArrow XmlTree [(String,String)]
getItemDatas = multi (hasName "itemDatas")
               >>>
               (listA $ getChildren
                       >>>
                       hasName "itemData"
                       >>>
                       getAttrValue "name" &&& getAttrValue "defStyleNum")

getLists :: IOSArrow XmlTree [(String, [String])]
getLists = listA $ multi (hasName "list")
                   >>>
                   getAttrValue "name" &&& getListContents

getListContents :: IOSArrow XmlTree [String]
getListContents = listA $ getChildren
                          >>>
                          hasName "item"
                          >>>
                          getChildren
                          >>>
                          getText
                          >>>
                          arr stripWhitespace

getContexts :: IOSArrow XmlTree [SyntaxContext]
getContexts = listA $   multi (hasName "context")
                        >>>
                        proc x -> do
                          name <- getAttrValue "name" -< x
                          attribute <- getAttrValue "attribute" -< x
                          lineEndContext <- getAttrValue "lineEndContext" -< x
                          lineBeginContext <- getAttrValue "lineBeginContext" -< x
                          fallthrough <- getAttrValue "fallthrough" -< x
                          fallthroughContext <- getAttrValue "fallthroughContext" -< x
                          dynamic <- getAttrValue "dynamic" -< x
                          parsers <- getParsers -< x
                          returnA -< SyntaxContext
                                           { contName = name
                                           , contAttribute = attribute
                                           , contLineEndContext = if null lineEndContext then "#stay" else lineEndContext
                                           , contLineBeginContext = if null lineBeginContext then "#stay" else lineBeginContext
                                           , contFallthrough = vBool False fallthrough
                                           , contFallthroughContext = if null fallthroughContext then "#pop" else fallthroughContext
                                           , contDynamic = vBool False dynamic
                                           , contParsers = parsers }

-- Note, some xml files have "\\" for a backslash,
-- others have "\".  Not sure what the rules are, but
-- this covers both bases:
readChar :: String -> Char
readChar s = case s of
                  [c] -> c
                  _   -> readDef '\xffff' $ "'" ++ s ++ "'"


getParsers :: IOSArrow XmlTree [Rule]
getParsers = listA $ getChildren
                     >>>
                     proc x -> do
                       name <- getName -< x
                       attribute <- getAttrValue "attribute" -< x
                       context <- getAttrValue "context" -< x
                       char0 <- arr readChar <<< getAttrValue "char" -< x
                       char1 <- arr readChar <<< getAttrValue "char1" -< x
                       str' <- getAttrValue "String" -< x
                       includeAttrib <- arr (vBool False) <<< getAttrValue "includeAttrib" -< x
                       lookahead <- arr (vBool False) <<< getAttrValue "lookAhead" -< x
                       firstNonSpace <- arr (vBool False) <<< getAttrValue "firstNonSpace" -< x
                       column' <- getAttrValue "column" -< x
                       dynamic <- arr (vBool False) <<< getAttrValue "dynamic" -< x
                       children <- getParsers -< x
                       let tildeRegex = name == "RegExpr" && take 1 str' == "^"
                       let str = if tildeRegex then drop 1 str' else str'
                       let column = if tildeRegex
                                       then Just 0
                                       else readMay column'
                       let matcher = case name of
                                          "DetectChar" -> DetectChar char0
                                          "Detect2Chars" -> Detect2Chars char0 char1
                                          "AnyChar" -> AnyChar str
                                          "RangeDetect" -> RangeDetect char0 char1
                                          "StringText" -> StringDetect (Text.pack str)
                                          "RegExpr" -> Unimplemented -- TODO
                                          "Keyword" -> Unimplemented -- TODO
                                          "Int" -> Int
                                          "Float" -> Float
                                          "HlCOct" -> HlCOct
                                          "HlCHex" -> HlCHex
                                          "HlCStringChar" -> HlCStringChar
                                          "LineContinue" -> Unimplemented -- TODO
                                          "IncludeRules" -> Unimplemented -- TODO
                                          "DetectSpaces" -> DetectSpaces
                                          "DetectIdentifier" -> DetectIdentifier
                                          _ -> Unimplemented -- TODO
                       let contextSwitch = [] -- TODO 
                       returnA -< Rule{ matcher = matcher,
                                        attribute = Text.pack attribute,
                                        dynamic = dynamic,
                                        children = children,
                                        contextSwitch = contextSwitch }



getKeywordAttrs :: IOSArrow XmlTree [SyntaxKeywordAttr]
getKeywordAttrs = listA $ multi $ hasName "keywords"
                                  >>>
                                  proc x -> do
                                    caseSensitive <- getAttrValue "casesensitive" -< x
                                    weakDelim <- getAttrValue "weakDeliminator" -< x
                                    additionalDelim <- getAttrValue "additionalDeliminator" -< x
                                    returnA -< SyntaxKeywordAttr
                                                      { keywordCaseSensitive = vBool True caseSensitive
                                                      , keywordDelims = (standardDelims ++ additionalDelim) \\ weakDelim }


