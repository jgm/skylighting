{-# LANGUAGE Arrows #-}

import Safe
import System.FilePath (takeBaseName, (</>), (<.>))
import Text.XML.HXT.Core
import Text.Show.Pretty (ppShow)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.List ((\\))
import Control.Monad
import Skylighting.Parser
import System.Environment (getArgs)
import qualified Data.Map as Map

{-
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
                   , synKeywordAttr   :: KeywordAttr
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


standardDelims :: Set.Set Char
standardDelims = Set.fromList " \n\t.():!+,-<=>%&*/;?[]^{|}~\\"

defaultKeywordAttr :: KeywordAttr
defaultKeywordAttr = KeywordAttr { keywordCaseSensitive = True
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
  syntaxes <- getArgs >>= (mapM (runX . application)) >>= return . mconcat
  let syntaxMap = Map.fromList [(sName s, s) | s <- syntaxes]
  putStrLn $
      "module Skylighting.Syntax (syntaxMap) where\nimport Skylighting.Parser\nimport Data.Map\nimport qualified Data.Set\n\nsyntaxMap :: Data.Map.Map String Syntax\nsyntaxMap = " ++ ppShow syntaxMap

application :: String -> IOSArrow b Syntax
application src
    = readDocument [withValidate no, withInputEncoding utf8] src
      >>>
      multi (hasName "language")
      >>>
      extractSyntaxDefinition

extractSyntaxDefinition :: IOSArrow XmlTree Syntax
extractSyntaxDefinition =  proc x -> do
                             lang <- getAttrValue "name" -< x
                             author <- getAttrValue "author" -< x
                             version <- getAttrValue "version" -< x
                             license <- getAttrValue "license" -< x
                             sources <- getAttrValue "extensions" -< x
                             caseSensitive <- arr (vBool True) <<< getAttrValue "casesensitive" -< x
                             itemdatas <- getItemDatas -< x
                             -- lists <- getLists -< x
                             -- keywordAttr <- arr (headDef defaultKeywordAttr) <<< getKeywordAttrs -< x
                             contexts <- getContexts $< getLists &&& (arr (headDef defaultKeywordAttr) <<< getKeywordAttrs) -< x
                             returnA -< Syntax{
                                          sName     = lang
                                        , sContexts = Map.fromList
                                               [(cName c, c) | c <- contexts]
                                        }

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

getContexts :: ([(String, [String])], KeywordAttr) -> IOSArrow XmlTree [Context]
getContexts (lists, kwattr) = listA $   multi (hasName "context")
                        >>>
                        proc x -> do
                          name <- getAttrValue "name" -< x
                          attribute <- getAttrValue "attribute" -< x
                          lineEndContext <- getAttrValue "lineEndContext" -< x
                          lineBeginContext <- getAttrValue "lineBeginContext" -< x
                          fallthrough <- getAttrValue "fallthrough" -< x
                          fallthroughContext <- getAttrValue "fallthroughContext" -< x
                          dynamic <- getAttrValue "dynamic" -< x
                          parsers <- getParsers (lists, kwattr) -< x
                          returnA -< Context {
                                        cName = name
                                      , cRules = parsers
                                      }
                                      {-
                                       SyntaxContext
                                           { contName = name
                                           , contAttribute = attribute
                                           , contLineEndContext = if null lineEndContext then "#stay" else lineEndContext
                                           , contLineBeginContext = if null lineBeginContext then "#stay" else lineBeginContext
                                           , contFallthrough = vBool False fallthrough
                                           , contFallthroughContext = if null fallthroughContext then "#pop" else fallthroughContext
                                           , contDynamic = vBool False dynamic
                                           , contParsers = parsers }
                                      -}

-- Note, some xml files have "\\" for a backslash,
-- others have "\".  Not sure what the rules are, but
-- this covers both bases:
readChar :: String -> Char
readChar s = case s of
                  [c] -> c
                  _   -> readDef '\xffff' $ "'" ++ s ++ "'"


getParsers :: ([(String, [String])], KeywordAttr) -> IOSArrow XmlTree [Rule]
getParsers (lists, kwattr) = listA $ getChildren
                     >>>
                     proc x -> do
                       name <- getName -< x
                       attribute <- getAttrValue "attribute" -< x
                       context <- getAttrValue "context" -< x
                       char0 <- arr readChar <<< getAttrValue "char" -< x
                       char1 <- arr readChar <<< getAttrValue "char1" -< x
                       str' <- getAttrValue "String" -< x
                       insensitive <- arr (vBool False) <<< getAttrValue "insensitive" -< x
                       includeAttrib <- arr (vBool False) <<< getAttrValue "includeAttrib" -< x
                       lookahead <- arr (vBool False) <<< getAttrValue "lookAhead" -< x
                       firstNonSpace <- arr (vBool False) <<< getAttrValue "firstNonSpace" -< x
                       column' <- getAttrValue "column" -< x
                       dynamic <- arr (vBool False) <<< getAttrValue "dynamic" -< x
                       children <- getParsers (lists, kwattr) -< x
                       let tildeRegex = name == "RegExpr" && take 1 str' == "^"
                       let str = if tildeRegex then drop 1 str' else str'
                       let column = if tildeRegex
                                       then Just 0
                                       else readMay column'
                       let compiledRe = if dynamic
                                           then Nothing
                                           else Just $ compileRegex True str
                       let re = RegExpr RE{ reString = str
                                          , reDynamic = dynamic
                                          , reCompiled = compiledRe
                                          , reCaseSensitive = not insensitive }
                       let (syntaxname, contextname) =
                               case break (=='#') context of
                                     (cont, '#':'#':lang) -> (Just lang, cont)
                                     _ -> (Nothing, context)
                       let matcher = case name of
                                          "DetectChar" -> DetectChar char0
                                          "Detect2Chars" -> Detect2Chars char0 char1
                                          "AnyChar" -> AnyChar str
                                          "RangeDetect" -> RangeDetect char0 char1
                                          "StringText" -> StringDetect str
                                          "RegExpr" -> re
                                          "keyword" -> Keyword kwattr $
                                             fromMaybe [] (lookup str lists)
                                          "Int" -> Int
                                          "Float" -> Float
                                          "HlCOct" -> HlCOct
                                          "HlCHex" -> HlCHex
                                          "HlCStringChar" -> HlCStringChar
                                          "LineContinue" -> LineContinue
                                          "IncludeRules" -> IncludeRules syntaxname contextname
                                          "DetectSpaces" -> DetectSpaces
                                          "DetectIdentifier" -> DetectIdentifier
                                          _ -> Unimplemented name
                       let contextSwitch = if name == "IncludeRules"
                                              then []
                                              else parseContextSwitch context
                       returnA -< Rule{ rMatcher = matcher,
                                        rAttribute = if includeAttrib
                                                        then ""
                                                        else attribute,
                                        rDynamic = dynamic,
                                        rChildren = children,
                                        rContextSwitch = contextSwitch }

parseContextSwitch :: String -> [ContextSwitch]
parseContextSwitch [] = []
parseContextSwitch "#stay" = []
parseContextSwitch ('#':'p':'o':'p':xs) = Pop : parseContextSwitch xs
parseContextSwitch ('!':xs) = [Push xs]
parseContextSwitch xs = [Push xs]

getKeywordAttrs :: IOSArrow XmlTree [KeywordAttr]
getKeywordAttrs = listA $ multi $ hasName "keywords"
                                  >>>
                                  proc x -> do
                                    caseSensitive <- arr (vBool True) <<< getAttrValue "casesensitive" -< x
                                    weakDelim <- getAttrValue "weakDeliminator" -< x
                                    additionalDelim <- getAttrValue "additionalDeliminator" -< x
                                    returnA -< KeywordAttr
                                                      { keywordCaseSensitive = caseSensitive
                                                      , keywordDelims = (Set.union standardDelims (Set.fromList additionalDelim)) Set.\\ Set.fromList weakDelim }


