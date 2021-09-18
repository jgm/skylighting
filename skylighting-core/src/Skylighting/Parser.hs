{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Skylighting.Parser ( parseSyntaxDefinition
                          , parseSyntaxDefinitionFromText
                          , addSyntaxDefinition
                          , missingIncludes
                          ) where

import qualified Data.String as String
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isAlphaNum, toUpper)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as TR
import qualified Data.Text.Encoding as TE
import Safe
import Skylighting.Regex
import Skylighting.Types
import System.FilePath
import Text.XML
import qualified Control.Exception as E
import Control.Monad.Except
import Control.Monad.Identity

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
parseSyntaxDefinition fp = do
  bs <- BL.readFile fp
  case parseText def (toTextLazy bs) of
    Left e    -> return $ Left $ E.displayException e
    Right doc -> runExceptT (documentToSyntax fp doc)
 where
  toTextLazy = TLE.decodeUtf8 . filterCRs . dropBOM
  dropBOM bs =
         if "\xEF\xBB\xBF" `BL.isPrefixOf` bs
            then BL.drop 3 bs
            else bs
  filterCRs = BL.filter (/='\r')

parseSyntaxDefinitionFromText ::
  FilePath -> TL.Text -> Either String Syntax
parseSyntaxDefinitionFromText fp xml =
    case parseText def xml of
      Left e    -> Left $ E.displayException e
      Right doc -> runIdentity $ runExceptT $ documentToSyntax fp doc

-- | Parses an XML 'Document' as a 'Syntax' description.
documentToSyntax :: Monad m
                 => FilePath -- ^ used for short name
                 -> Document
                 -> ExceptT String m Syntax
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

  let defKeywordAttr = getKeywordAttrs rootEl

  let contextEls = getElementsNamed "contexts" hlEl >>=
                   getElementsNamed "context"

  let syntaxname = getAttrValue "name" rootEl

  contexts <- mapM
    (getContext casesensitive syntaxname itemDatas lists defKeywordAttr)
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
           , sLists      = lists
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

getList :: Monad m => Element -> ExceptT String m (Text, [ListItem])
getList el = do
  case M.lookup "name" (elementAttributes el) of
    Nothing   -> throwError "No name attribute on list"
    Just name -> (name,) <$>
                   mapM toListItem [el' | NodeElement el' <- elementNodes el]
 where
   toListItem el' = case elementName el' of
                      "item"    -> return $ Item $ T.strip $ getTextContent el'
                      "include" -> do
                        let (syntaxname, listname) =
                             case T.breakOn "##"
                                   (T.strip (getTextContent el')) of
                               (x ,y) | T.null y  -> ("", x)
                                      | otherwise -> (T.drop 2 y, x)
                        return $ IncludeList (syntaxname, listname)
                      x -> throwError $ "Unknown element " ++ show x ++
                                        " in list"

getParser :: Monad m
          => Bool -> Text -> ItemData -> M.Map Text [ListItem] -> KeywordAttr
          -> Text -> Element -> ExceptT String m Rule
getParser casesensitive syntaxname itemdatas lists kwattr cattr el = do
  let name = nameLocalName . elementName $ el
  let attribute = getAttrValue "attribute" el
  let context = getAttrValue "context" el
  let char0 = readChar $ getAttrValue "char" el
  let char1 = readChar $ getAttrValue "char1" el
  let str' = getAttrValue "String" el
  let insensitive = vBool (not casesensitive) $ getAttrValue "insensitive" el
  let includeAttrib = vBool False $ getAttrValue "includeAttrib" el
  let lookahead = vBool False $ getAttrValue "lookAhead" el
  let firstNonSpace = vBool False $ getAttrValue "firstNonSpace" el
  let column' = getAttrValue "column" el
  let dynamic = vBool False $ getAttrValue "dynamic" el
  children <- mapM (getParser casesensitive
                    syntaxname itemdatas lists kwattr attribute)
                  [e | NodeElement e <- elementNodes el ]
  let tildeRegex = name == "RegExpr" && T.take 1 str' == "^"
  let str = if tildeRegex then T.drop 1 str' else str'
  let column = if tildeRegex
                  then Just (0 :: Int)
                  else either (\_ -> Nothing) (Just . fst) $
                         TR.decimal column'
  let re = RegExpr RE{ reString = TE.encodeUtf8 str
                     , reCaseSensitive = not insensitive }
  let (incsyntax, inccontext) =
          case T.breakOn "##" context of
                (_,x) | T.null x -> (syntaxname, context)
                (cont, lang)     -> (T.drop 2 lang, cont)
  matcher <- case name of
                 "DetectChar" -> return $ DetectChar char0
                 "Detect2Chars" -> return $ Detect2Chars char0 char1
                 "AnyChar" -> return $ AnyChar $ Set.fromList $ T.unpack str
                 "RangeDetect" -> return $ RangeDetect char0 char1
                 "StringDetect" -> return $ StringDetect str
                 "WordDetect" -> return $ WordDetect str
                 "RegExpr" -> return $ re
                 "keyword" ->
                    case M.lookup str lists of
                      Nothing -> throwError $ "List not found: " ++ T.unpack str
                      Just lst -> return $ Keyword kwattr
                            (makeWordSet (keywordCaseSensitive kwattr)
                              [t | Item t <- lst])
                 "Int" -> return $ Int
                 "Float" -> return $ Float
                 "HlCOct" -> return $ HlCOct
                 "HlCHex" -> return $ HlCHex
                 "HlCStringChar" -> return $ HlCStringChar
                 "HlCChar" -> return $ HlCChar
                 "LineContinue" -> return $ LineContinue
                 "IncludeRules" -> return $
                   IncludeRules (incsyntax, inccontext)
                 "DetectSpaces" -> return $ DetectSpaces
                 "DetectIdentifier" -> return $ DetectIdentifier
                 _ -> throwError $ "Unknown element " ++ T.unpack name

  let contextSwitch = if name == "IncludeRules"
                         then []  -- is this right?
                         else parseContextSwitch incsyntax inccontext
  return $ Rule{ rMatcher = matcher
               , rAttribute = fromMaybe NormalTok $
                    if T.null attribute
                       then M.lookup cattr itemdatas
                       else M.lookup attribute itemdatas
               , rIncludeAttribute = includeAttrib
               , rDynamic = dynamic
               , rCaseSensitive = not insensitive
               , rChildren = children
               , rContextSwitch = contextSwitch
               , rLookahead = lookahead
               , rFirstNonspace = firstNonSpace
               , rColumn = column
               }


getContext :: Monad m
           => Bool
           -> Text
           -> ItemData
           -> M.Map Text [ListItem]
           -> KeywordAttr
           -> Element
           -> ExceptT String m Context
getContext casesensitive syntaxname itemDatas lists kwattr el = do
  let name = getAttrValue "name" el
  let attribute = getAttrValue "attribute" el
  let lineEmptyContext = getAttrValue "lineEmptyContext" el
  let lineEndContext = getAttrValue "lineEndContext" el
  let lineBeginContext = getAttrValue "lineBeginContext" el
  let fallthrough = vBool False $ getAttrValue "fallthrough" el
  let fallthroughContext = getAttrValue "fallthroughContext" el
  let dynamic = vBool False $ getAttrValue "dynamic" el

  parsers <- mapM (getParser casesensitive
                    syntaxname itemDatas lists kwattr attribute)
                  [e | NodeElement e <- elementNodes el ]

  return $ Context {
            cName = name
          , cSyntax = syntaxname
          , cRules = parsers
          , cAttribute = fromMaybe NormalTok $ M.lookup attribute itemDatas
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

getItemData :: Element -> ItemData
getItemData el = toItemDataTable $
  [(getAttrValue "name" e, getAttrValue "defStyleNum" e)
    | e <- (getElementsNamed "itemDatas" el >>= getElementsNamed "itemData")
  ]

getKeywordAttrs :: Element -> KeywordAttr
getKeywordAttrs el =
  case (getElementsNamed "general" el >>= getElementsNamed "keywords") of
     []    -> defaultKeywordAttr
     (x:_) ->
       let weakDelim = T.unpack $ getAttrValue "weakDeliminator" x
           additionalDelim = T.unpack $ getAttrValue "additionalDeliminator" x
        in KeywordAttr { keywordCaseSensitive =
                             vBool True $ getAttrValue "casesensitive" x
                       , keywordDelims = Set.union standardDelims
                           (Set.fromList additionalDelim) Set.\\
                             Set.fromList weakDelim }

parseContextSwitch :: Text -> Text -> [ContextSwitch]
parseContextSwitch syntaxname t =
  if T.null t || t == "#stay"
     then []
     else
       case T.stripPrefix "#pop" t of
         Just rest -> Pop : parseContextSwitch syntaxname rest
         Nothing   -> [Push (syntaxname, T.dropWhile (=='!') t)]

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

-- Note, some xml files have "\\" for a backslash,
-- others have "\".  Not sure what the rules are, but
-- this covers both bases:
readChar :: Text -> Char
readChar t = case T.unpack t of
                  [c] -> c
                  s   -> readDef '\xffff' $ "'" ++ s ++ "'"

pathToLangName :: String -> String
pathToLangName s = capitalize (camelize (takeBaseName s))

camelize :: String -> String
camelize (d:c:cs) | not (isAlphaNum d) = toUpper c : camelize cs
camelize (c:cs)   = c : camelize cs
camelize []       = []

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize []     = []
