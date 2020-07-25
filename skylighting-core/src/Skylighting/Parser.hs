{-# LANGUAGE Arrows #-}
module Skylighting.Parser ( parseSyntaxDefinition
                          , parseSyntaxDefinitionFromString
                          , addSyntaxDefinition
                          , missingIncludes
                          ) where

import Data.ByteString.UTF8 (fromString, toString)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, toUpper)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Safe
import Skylighting.Regex
import Skylighting.Types
import System.FilePath
import Text.XML.HXT.Core

-- | Adds a syntax definition to a syntax map,
-- replacing any existing definition with the same name.
addSyntaxDefinition :: Syntax -> SyntaxMap -> SyntaxMap
addSyntaxDefinition s = Map.insert (sName s) s

-- | Scan a list of 'Syntax's and make sure that
-- `IncludeRules` never asks for a syntax not in this
-- list.  Produces a list of pairs where the first
-- element is the including syntax name and the second
-- element is the (missing) included syntax name.
-- This is intended for sanity checks to avoid run-time
-- errors.
missingIncludes :: [Syntax] -> [(Text, Text)]
missingIncludes syns = ordNub
  [(sName s, lang)
     | s <- syns
     , c <- Map.elems (sContexts s)
     , IncludeRules (lang, _) <- map rMatcher (cRules c)
     , not (lang `Set.member` syntaxNames)]
   where syntaxNames = Set.fromList $ map sName syns

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

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

-- | Parses a file containing a Kate XML syntax definition
-- into a 'Syntax' description.  Note that if the DOCTYPE contains
-- a reference to the now-obsolete language.dtd, we remove it.
parseSyntaxDefinition :: FilePath -> IO (Either String Syntax)
parseSyntaxDefinition fp = do
  xml <- toString <$> BS.readFile fp
  parseSyntaxDefinitionFromString fp xml

-- | Parses a string containing a Kate XML syntax definition
-- into a 'Syntax' description.  Note that if the DOCTYPE contains
-- a reference to the now-obsolete language.dtd, we remove it.
parseSyntaxDefinitionFromString :: FilePath -- ^ used for short name
                                -> String
                                -> IO (Either String Syntax)
parseSyntaxDefinitionFromString fp xml = do
  res <- runX ( readString [withValidate no]
                  (removeLanguageDTD . removeBOM $ xml)
                >>>
                application fp )
  case res of
       [s] -> return $ Right s
       _   -> return $ Left $ "Could not parse syntax definition " ++ fp

removeBOM :: String -> String
removeBOM ('\xFEFF':xs) = xs
removeBOM xs            = xs

removeLanguageDTD :: String -> String
removeLanguageDTD ('S':'Y':'S':'T':'E':'M':' ':xs) =
  removeLanguageDTD $ dropWhile (\c -> c /= '[' && c /= '>') xs
removeLanguageDTD xs@('<':'l':'a':'n':'g':_) = xs
removeLanguageDTD (x:xs) = x : removeLanguageDTD xs
removeLanguageDTD [] = []

application :: FilePath -> IOSArrow XmlTree Syntax
application fp
    = multi (hasName "language")
      >>>
      extractSyntaxDefinition (takeFileName fp)

extractSyntaxDefinition :: String -> IOSArrow XmlTree Syntax
extractSyntaxDefinition filename =
  proc x -> do
     lang <- getAttrValue "name" -< x
     author <- getAttrValue "author" -< x
     version <- getAttrValue "version" -< x
     license <- getAttrValue "license" -< x
     extensions <- getAttrValue "extensions" -< x
     contexts <- getContexts $<
                    (arr (vBool True) <<< getAttrValue "casesensitive") &&&
                    (getAttrValue "name") &&&
                    (arr toItemDataTable <<< getItemDatas) &&&
                    getLists &&&
                    (arr (headDef defaultKeywordAttr) <<< getKeywordAttrs) -< x
     startingContext <- case contexts of
                             (c:_) -> returnA -< cName c
                             []    -> issueErr "No contexts" >>> none -< ()
     returnA -< Syntax{
                  sName     = Text.pack lang
                , sFilename = filename
                , sShortname = Text.pack $ pathToLangName filename
                , sAuthor   = Text.pack $ author
                , sVersion  = Text.pack $ version
                , sLicense  = Text.pack $ license
                , sExtensions = words $ map
                     (\c -> if c == ';'
                               then ' '
                               else c) extensions
                , sContexts = Map.fromList
                       [(cName c, c) | c <- contexts]
                , sStartingContext = startingContext
                }

toItemDataTable :: [(String,String)] -> Map.Map String TokenType
toItemDataTable = Map.fromList . map (\(s,t) -> (s, toTokenType t))

getItemDatas :: IOSArrow XmlTree [(String,String)]
getItemDatas =
  multi (hasName "itemDatas")
     >>>
     (listA $ getChildren
             >>>
             hasName "itemData"
             >>>
             getAttrValue "name" &&& getAttrValue "defStyleNum")

toTokenType :: String -> TokenType
toTokenType s =
  case s of
       "dsNormal"         -> NormalTok
       "dsKeyword"        -> KeywordTok
       "dsDataType"       -> DataTypeTok
       "dsDecVal"         -> DecValTok
       "dsBaseN"          -> BaseNTok
       "dsFloat"          -> FloatTok
       "dsConstant"       -> ConstantTok
       "dsChar"           -> CharTok
       "dsSpecialChar"    -> SpecialCharTok
       "dsString"         -> StringTok
       "dsVerbatimString" -> VerbatimStringTok
       "dsSpecialString"  -> SpecialStringTok
       "dsImport"         -> ImportTok
       "dsComment"        -> CommentTok
       "dsDocumentation"  -> DocumentationTok
       "dsAnnotation"     -> AnnotationTok
       "dsCommentVar"     -> CommentVarTok
       "dsOthers"         -> OtherTok
       "dsFunction"       -> FunctionTok
       "dsVariable"       -> VariableTok
       "dsControlFlow"    -> ControlFlowTok
       "dsOperator"       -> OperatorTok
       "dsBuiltIn"        -> BuiltInTok
       "dsExtension"      -> ExtensionTok
       "dsPreprocessor"   -> PreprocessorTok
       "dsAttribute"      -> AttributeTok
       "dsRegionMarker"   -> RegionMarkerTok
       "dsInformation"    -> InformationTok
       "dsWarning"        -> WarningTok
       "dsAlert"          -> AlertTok
       "dsError"          -> ErrorTok
       _                  -> NormalTok

getLists :: IOSArrow XmlTree [(String, [String])]
getLists =
  listA $ multi (hasName "list")
     >>>
     getAttrValue "name" &&& getListContents

getListContents :: IOSArrow XmlTree [String]
getListContents =
  listA $ getChildren
     >>>
     hasName "item"
     >>>
     getChildren
     >>>
     getText
     >>>
     arr stripWhitespace

getContexts ::
     (Bool,
       (String,
         (Map.Map String TokenType,
           ([(String, [String])], KeywordAttr))))
            -> IOSArrow XmlTree [Context]
getContexts (casesensitive, (syntaxname, (itemdatas, (lists, kwattr)))) =
  listA $ multi (hasName "context")
     >>>
     proc x -> do
       name <- getAttrValue "name" -< x
       attribute <- getAttrValue "attribute" -< x
       lineEmptyContext <- getAttrValue "lineEmptyContext" -< x
       lineEndContext <- getAttrValue "lineEndContext" -< x
       lineBeginContext <- getAttrValue "lineBeginContext" -< x
       fallthrough <- arr (vBool False) <<< getAttrValue "fallthrough" -< x
       fallthroughContext <- getAttrValue "fallthroughContext" -< x
       dynamic <- arr (vBool False) <<< getAttrValue "dynamic" -< x
       parsers <- getParsers (casesensitive, (syntaxname,
                                (itemdatas, (lists, kwattr)))) $<
                            getAttrValue "attribute" -< x
       returnA -< Context {
                     cName = Text.pack name
                   , cSyntax = Text.pack syntaxname
                   , cRules = parsers
                   , cAttribute = fromMaybe NormalTok $
                           Map.lookup attribute itemdatas
                   , cLineEmptyContext =
                        parseContextSwitch syntaxname lineEmptyContext
                   , cLineEndContext =
                        parseContextSwitch syntaxname lineEndContext
                   , cLineBeginContext =
                        parseContextSwitch syntaxname lineBeginContext
                   , cFallthrough = fallthrough
                   , cFallthroughContext =
                        parseContextSwitch syntaxname fallthroughContext
                   , cDynamic = dynamic
                   }

-- Note, some xml files have "\\" for a backslash,
-- others have "\".  Not sure what the rules are, but
-- this covers both bases:
readChar :: String -> Char
readChar s = case s of
                  [c] -> c
                  _   -> readDef '\xffff' $ "'" ++ s ++ "'"


getParsers :: (Bool,
                (String,
                  (Map.Map String TokenType,
                    ([(String, [String])], KeywordAttr))))
            -> String  -- context attribute
            -> IOSArrow XmlTree [Rule]
getParsers (casesensitive, (syntaxname, (itemdatas, (lists, kwattr)))) cattr =
  listA $ getChildren
     >>>
     proc x -> do
       name <- getName -< x
       attribute <- getAttrValue "attribute" -< x
       context <- getAttrValue "context" -< x
       char0 <- arr readChar <<< getAttrValue "char" -< x
       char1 <- arr readChar <<< getAttrValue "char1" -< x
       str' <- getAttrValue "String" -< x
       insensitive <- arr (vBool (not casesensitive))
                            <<< getAttrValue "insensitive" -< x
       includeAttrib <- arr (vBool False) <<< getAttrValue "includeAttrib" -< x
       lookahead <- arr (vBool False) <<< getAttrValue "lookAhead" -< x
       firstNonSpace <- arr (vBool False) <<< getAttrValue "firstNonSpace" -< x
       column' <- getAttrValue "column" -< x
       dynamic <- arr (vBool False) <<< getAttrValue "dynamic" -< x
       children <- getParsers
                     (casesensitive, (syntaxname, (itemdatas, (lists, kwattr))))
                     cattr -< x
       let tildeRegex = name == "RegExpr" && take 1 str' == "^"
       let str = if tildeRegex then drop 1 str' else str'
       let column = if tildeRegex
                       then Just (0 :: Int)
                       else readMay column'
       let re = RegExpr RE{ reString = fromString str
                          , reCaseSensitive = not insensitive }
       let (incsyntax, inccontext) =
               case break (=='#') context of
                     (cont, '#':'#':lang) -> (lang, cont)
                     _                    -> (syntaxname, context)
       let mbmatcher = case name of
                         "DetectChar" -> Just $ DetectChar char0
                         "Detect2Chars" -> Just $ Detect2Chars char0 char1
                         "AnyChar" -> Just $ AnyChar $ Set.fromList str
                         "RangeDetect" -> Just $ RangeDetect char0 char1
                         "StringDetect" -> Just $ StringDetect $ Text.pack str
                         "WordDetect" -> Just $ WordDetect $ Text.pack str
                         "RegExpr" -> Just $ re
                         "keyword" -> Just $ Keyword kwattr $
                            maybe (makeWordSet True [])
                              (makeWordSet (keywordCaseSensitive kwattr) . map Text.pack)
                              (lookup str lists)
                         "Int" -> Just $ Int
                         "Float" -> Just $ Float
                         "HlCOct" -> Just $ HlCOct
                         "HlCHex" -> Just $ HlCHex
                         "HlCStringChar" -> Just $ HlCStringChar
                         "HlCChar" -> Just $ HlCChar
                         "LineContinue" -> Just $ LineContinue
                         "IncludeRules" -> Just $
                           IncludeRules (Text.pack incsyntax, Text.pack inccontext)
                         "DetectSpaces" -> Just $ DetectSpaces
                         "DetectIdentifier" -> Just $ DetectIdentifier
                         _ -> Nothing

       matcher <- case mbmatcher of
                       Nothing -> none
                                   <<< applyA (arr issueWarn)
                                   <<< arr ("Unknown element " ++)
                                   <<< getName -< x
                       Just m  -> returnA -< m

       let contextSwitch = if name == "IncludeRules"
                              then []  -- is this right?
                              else parseContextSwitch incsyntax inccontext
       returnA -< Rule{ rMatcher = matcher,
                        rAttribute = fromMaybe NormalTok $
                           if null attribute
                              then Map.lookup cattr itemdatas
                              else Map.lookup attribute itemdatas,
                        rIncludeAttribute = includeAttrib,
                        rDynamic = dynamic,
                        rCaseSensitive = not insensitive,
                        rChildren = children,
                        rContextSwitch = contextSwitch,
                        rLookahead = lookahead,
                        rFirstNonspace = firstNonSpace ,
                        rColumn = column }

parseContextSwitch :: String -> String -> [ContextSwitch]
parseContextSwitch _ [] = []
parseContextSwitch _ "#stay" = []
parseContextSwitch syntaxname ('#':'p':'o':'p':xs) =
  Pop : parseContextSwitch syntaxname xs
parseContextSwitch syntaxname ('!':xs) = [Push (Text.pack syntaxname, Text.pack xs)]
parseContextSwitch syntaxname xs = [Push (Text.pack syntaxname, Text.pack xs)]

getKeywordAttrs :: IOSArrow XmlTree [KeywordAttr]
getKeywordAttrs =
  listA $ multi $ hasName "keywords"
     >>>
     proc x -> do
       caseSensitive <- arr (vBool True) <<< getAttrValue "casesensitive" -< x
       weakDelim <- getAttrValue "weakDeliminator" -< x
       additionalDelim <- getAttrValue "additionalDeliminator" -< x
       returnA -< KeywordAttr
                         { keywordCaseSensitive = caseSensitive
                         , keywordDelims = (Set.union standardDelims
                             (Set.fromList additionalDelim)) Set.\\
                                Set.fromList weakDelim }

pathToLangName :: String -> String
pathToLangName s = capitalize (camelize (takeBaseName s))

camelize :: String -> String
camelize (d:c:cs) | not (isAlphaNum d) = toUpper c : camelize cs
camelize (c:cs)   = c : camelize cs
camelize []       = []

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize []     = []
