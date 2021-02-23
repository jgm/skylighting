{-# LANGUAGE OverloadedStrings #-}
module Skylighting.Parser ( parseSyntaxDefinition
                          , parseSyntaxDefinitionFromLBS
                          , addSyntaxDefinition
                          , missingIncludes
                          ) where

import qualified Data.String as String
import Data.ByteString.UTF8 (fromString, toString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, toUpper)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Safe
import Skylighting.Regex
import Skylighting.Types
import System.FilePath
import Text.XML
import qualified Control.Exception as E
import Control.Monad.Except

-- | Adds a syntax definition to a syntax map,
-- replacing any existing definition with the same name.
addSyntaxDefinition :: Syntax -> SyntaxMap -> SyntaxMap
addSyntaxDefinition s = M.insert (sName s) s

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
     , c <- M.elems (sContexts s)
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

vBool :: Bool -> Text -> Bool
vBool defaultVal value = case value of
                           "true"  -> True
                           "yes"   -> True
                           "1"     -> True
                           "false" -> False
                           "no"    -> False
                           "0"     -> False
                           _       -> defaultVal

-- | Parses a file containing a Kate XML syntax definition
-- into a 'Syntax' description.
parseSyntaxDefinition :: FilePath -> IO (Either String Syntax)
parseSyntaxDefinition fp =
  parseSyntaxDefinitionFromLBS fp <$> BL.readFile fp

parseSyntaxDefinitionFromLBS ::
  FilePath -> BL.ByteString -> Either String Syntax
parseSyntaxDefinitionFromLBS fp xml =
    case parseLBS def xml of
      Left e    -> Left $ E.displayException e
      Right doc -> runExcept $ documentToSyntax fp doc

-- | Parses an XML 'Document' as a 'Syntax' description.
documentToSyntax :: FilePath -- ^ used for short name
                 -> Document
                 -> Except String Syntax
documentToSyntax fp Document{ documentRoot = rootEl } = do
  unless (elementName rootEl == "language") $
    throwError "Root element is not language"
  let filename = takeFileName fp
  let casesensitive = vBool True $ getAttrValue "casesensitive" rootEl

  hlEl <- case getElementsNamed "highlighting" rootEl of
            []      -> throwError "No highlighting element"
            (hl:_)  -> return hl

  lists <- M.fromList <$> mapM getList (getElementsNamed "list" hlEl)

  let itemDatas = getItemData hlEl

  defKeywordAttr <- undefined

  contextEls <- case getElementsNamed "contexts" rootEl of
                  []  -> throwError "No contexts element"
                  (cel:_) -> return $ getElementsNamed "context" cel
  contexts <- mapM
    (getContext casesensitive filename itemDatas lists defKeywordAttr)
    contextEls

  startingContext <- case contexts of
                       (c:_) -> return $ cName c
                       []    -> throwError "No contexts"

  return Syntax{
             sName       = getAttrValue "name" rootEl
           , sFilename   = filename
           , sShortname  = T.pack $ pathToLangName filename
           , sAuthor     = getAttrValue "author" rootEl
           , sVersion    = getAttrValue "version" rootEl
           , sLicense    = getAttrValue "license" rootEl
           , sExtensions = words $ map (\c -> if c == ';' then ' ' else c)
                                 $ T.unpack
                                 $ getAttrValue "extensions" rootEl
           , sContexts   = M.fromList
                  [(cName c, c) | c <- contexts]
           , sStartingContext = startingContext
           }

elementNamed :: String -> Node -> Bool
elementNamed name (NodeElement el) = elementName el == String.fromString name
elementNamed _ _ = False

getElementsNamed :: String -> Element -> [Element]
getElementsNamed name node =
  [el | NodeElement el <- filter (elementNamed name) (elementNodes node)]

getAttrValue :: String -> Element -> Text
getAttrValue key el = fromMaybe mempty $ M.lookup (String.fromString key)
                                       $ elementAttributes el

getTextContent :: Element -> Text
getTextContent el =
  mconcat [t | NodeContent t <- elementNodes el]

getList :: Element -> Except String (Text, [Text])
getList el = do
  case M.lookup "name" (elementAttributes el) of
    Nothing   -> throwError "No name attribute on list"
    Just name ->
      return (name, map (T.strip . getTextContent)
                        (getElementsNamed "item" el))


getContext :: Bool
           -> FilePath
           -> ItemData
           -> M.Map Text [Text]
           -> KeywordAttr
           -> Element
           -> Except String Context
getContext casesensitive name itemDatas lists defKeywordAttr = do
  undefined

getItemData :: Element -> ItemData
getItemData el = toItemDataTable $
  [(getAttrValue "name" el, getAttrValue "defStyleNum" el)
    | el <- (getElementsNamed "itemDatas" el >>= getElementsNamed "itemData")
  ]


-- getItemDatas :: IOSArrow XmlTree [(String,String)]
--
-- getItemDatas =
--   multi (hasName "itemDatas")
--      >>>
--      (listA $ getChildren
--              >>>
--              hasName "itemData"
--              >>>
--              getAttrValue "name" &&& getAttrValue "defStyleNum")
-- 
-- 
-- getContexts ::
--      (Bool,
--        (String,
--          (M.Map String TokenType,
--            ([(String, [String])], KeywordAttr))))
--             -> IOSArrow XmlTree [Context]
-- getContexts (casesensitive, (syntaxname, (itemdatas, (lists, kwattr)))) =
--   listA $ multi (hasName "context")
--      >>>
--      proc x -> do
--        name <- getAttrValue "name" -< x
--        attribute <- getAttrValue "attribute" -< x
--        lineEmptyContext <- getAttrValue "lineEmptyContext" -< x
--        lineEndContext <- getAttrValue "lineEndContext" -< x
--        lineBeginContext <- getAttrValue "lineBeginContext" -< x
--        fallthrough <- arr (vBool False) <<< getAttrValue "fallthrough" -< x
--        fallthroughContext <- getAttrValue "fallthroughContext" -< x
--        dynamic <- arr (vBool False) <<< getAttrValue "dynamic" -< x
--        parsers <- getParsers (casesensitive, (syntaxname,
--                                 (itemdatas, (lists, kwattr)))) $<
--                             getAttrValue "attribute" -< x
--        returnA -< Context {
--                      cName = T.pack name
--                    , cSyntax = T.pack syntaxname
--                    , cRules = parsers
--                    , cAttribute = fromMaybe NormalTok $
--                            M.lookup attribute itemdatas
--                    , cLineEmptyContext =
--                         parseContextSwitch syntaxname lineEmptyContext
--                    , cLineEndContext =
--                         parseContextSwitch syntaxname lineEndContext
--                    , cLineBeginContext =
--                         parseContextSwitch syntaxname lineBeginContext
--                    , cFallthrough = fallthrough
--                    , cFallthroughContext =
--                         parseContextSwitch syntaxname fallthroughContext
--                    , cDynamic = dynamic
--                    }
-- 
-- -- Note, some xml files have "\\" for a backslash,
-- -- others have "\".  Not sure what the rules are, but
-- -- this covers both bases:
-- readChar :: String -> Char
-- readChar s = case s of
--                   [c] -> c
--                   _   -> readDef '\xffff' $ "'" ++ s ++ "'"
-- 
-- 
-- getParsers :: (Bool,
--                 (String,
--                   (M.Map String TokenType,
--                     ([(String, [String])], KeywordAttr))))
--             -> String  -- context attribute
--             -> IOSArrow XmlTree [Rule]
-- getParsers (casesensitive, (syntaxname, (itemdatas, (lists, kwattr)))) cattr =
--   listA $ getChildren
--      >>>
--      proc x -> do
--        name <- getName -< x
--        attribute <- getAttrValue "attribute" -< x
--        context <- getAttrValue "context" -< x
--        char0 <- arr readChar <<< getAttrValue "char" -< x
--        char1 <- arr readChar <<< getAttrValue "char1" -< x
--        str' <- getAttrValue "String" -< x
--        insensitive <- arr (vBool (not casesensitive))
--                             <<< getAttrValue "insensitive" -< x
--        includeAttrib <- arr (vBool False) <<< getAttrValue "includeAttrib" -< x
--        lookahead <- arr (vBool False) <<< getAttrValue "lookAhead" -< x
--        firstNonSpace <- arr (vBool False) <<< getAttrValue "firstNonSpace" -< x
--        column' <- getAttrValue "column" -< x
--        dynamic <- arr (vBool False) <<< getAttrValue "dynamic" -< x
--        children <- getParsers
--                      (casesensitive, (syntaxname, (itemdatas, (lists, kwattr))))
--                      cattr -< x
--        let tildeRegex = name == "RegExpr" && take 1 str' == "^"
--        let str = if tildeRegex then drop 1 str' else str'
--        let column = if tildeRegex
--                        then Just (0 :: Int)
--                        else readMay column'
--        let re = RegExpr RE{ reString = fromString str
--                           , reCaseSensitive = not insensitive }
--        let (incsyntax, inccontext) =
--                case break (=='#') context of
--                      (cont, '#':'#':lang) -> (lang, cont)
--                      _                    -> (syntaxname, context)
--        let mbmatcher = case name of
--                          "DetectChar" -> Just $ DetectChar char0
--                          "Detect2Chars" -> Just $ Detect2Chars char0 char1
--                          "AnyChar" -> Just $ AnyChar $ Set.fromList str
--                          "RangeDetect" -> Just $ RangeDetect char0 char1
--                          "StringDetect" -> Just $ StringDetect $ T.pack str
--                          "WordDetect" -> Just $ WordDetect $ T.pack str
--                          "RegExpr" -> Just $ re
--                          "keyword" -> Just $ Keyword kwattr $
--                             maybe (makeWordSet True [])
--                               (makeWordSet (keywordCaseSensitive kwattr) . map T.pack)
--                               (lookup str lists)
--                          "Int" -> Just $ Int
--                          "Float" -> Just $ Float
--                          "HlCOct" -> Just $ HlCOct
--                          "HlCHex" -> Just $ HlCHex
--                          "HlCStringChar" -> Just $ HlCStringChar
--                          "HlCChar" -> Just $ HlCChar
--                          "LineContinue" -> Just $ LineContinue
--                          "IncludeRules" -> Just $
--                            IncludeRules (T.pack incsyntax, T.pack inccontext)
--                          "DetectSpaces" -> Just $ DetectSpaces
--                          "DetectIdentifier" -> Just $ DetectIdentifier
--                          _ -> Nothing
-- 
--        matcher <- case mbmatcher of
--                        Nothing -> none
--                                    <<< applyA (arr issueWarn)
--                                    <<< arr ("Unknown element " ++)
--                                    <<< getName -< x
--                        Just m  -> returnA -< m
-- 
--        let contextSwitch = if name == "IncludeRules"
--                               then []  -- is this right?
--                               else parseContextSwitch incsyntax inccontext
--        returnA -< Rule{ rMatcher = matcher,
--                         rAttribute = fromMaybe NormalTok $
--                            if null attribute
--                               then M.lookup cattr itemdatas
--                               else M.lookup attribute itemdatas,
--                         rIncludeAttribute = includeAttrib,
--                         rDynamic = dynamic,
--                         rCaseSensitive = not insensitive,
--                         rChildren = children,
--                         rContextSwitch = contextSwitch,
--                         rLookahead = lookahead,
--                         rFirstNonspace = firstNonSpace ,
--                         rColumn = column }
-- 
-- getKeywordAttrs :: IOSArrow XmlTree [KeywordAttr]
-- getKeywordAttrs =
--   listA $ multi $ hasName "keywords"
--      >>>
--      proc x -> do
--        caseSensitive <- arr (vBool True) <<< getAttrValue "casesensitive" -< x
--        weakDelim <- getAttrValue "weakDeliminator" -< x
--        additionalDelim <- getAttrValue "additionalDeliminator" -< x
--        returnA -< KeywordAttr
--                          { keywordCaseSensitive = caseSensitive
--                          , keywordDelims = (Set.union standardDelims
--                              (Set.fromList additionalDelim)) Set.\\
--                                 Set.fromList weakDelim }
--

parseContextSwitch :: String -> String -> [ContextSwitch]
parseContextSwitch _ [] = []
parseContextSwitch _ "#stay" = []
parseContextSwitch syntaxname ('#':'p':'o':'p':xs) =
  Pop : parseContextSwitch syntaxname xs
parseContextSwitch syntaxname ('!':xs) = [Push (T.pack syntaxname, T.pack xs)]
parseContextSwitch syntaxname xs = [Push (T.pack syntaxname, T.pack xs)]

type ItemData = M.Map Text TokenType

toItemDataTable :: [(Text, Text)] -> ItemData
toItemDataTable = M.fromList . map (\(s,t) -> (s, toTokenType t))

toTokenType :: Text -> TokenType
toTokenType t =
  case t of
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

pathToLangName :: String -> String
pathToLangName s = capitalize (camelize (takeBaseName s))
 
camelize :: String -> String
camelize (d:c:cs) | not (isAlphaNum d) = toUpper c : camelize cs
camelize (c:cs)   = c : camelize cs
camelize []       = []

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize []     = []
