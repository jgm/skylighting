{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Skylighting.Tokenizer (
    tokenize
  , TokenizerConfig(..)
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.CaseInsensitive (mk)
import Data.Char (isAlphaNum, isAscii, isDigit, isLetter, isSpace, ord)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Debug.Trace
import Skylighting.Regex
import Skylighting.Types
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

newtype ContextStack = ContextStack{ unContextStack :: [Context] }
  deriving (Show)

data TokenizerState = TokenizerState{
    input               :: ByteString
  , endline             :: Bool
  , prevChar            :: Char
  , contextStack        :: ContextStack
  , captures            :: IntMap.IntMap ByteString
  , column              :: Int
  , lineContinuation    :: Bool
  , firstNonspaceColumn :: Maybe Int
  , compiledRegexes     :: Map.Map RE Regex
}

-- | Configuration options for 'tokenize'.
data TokenizerConfig = TokenizerConfig{
    syntaxMap   :: SyntaxMap  -- ^ Syntax map to use
  , traceOutput :: Bool       -- ^ Generate trace output for debugging
} deriving (Show)

data Result e a = Success a
                | Failure
                | Error e
     deriving (Functor)

deriving instance (Show a, Show e) => Show (Result e a)

data TokenizerM a = TM { runTokenizerM :: TokenizerConfig
                                       -> TokenizerState
                                       -> (TokenizerState, Result String a) }

mapsnd :: (a -> b) -> (c, a) -> (c, b)
mapsnd f (x, y) = (x, f y)

instance Functor TokenizerM where
  fmap f (TM g) = TM (\c s -> mapsnd (fmap f) (g c s))

instance Applicative TokenizerM where
  pure x = TM (\_ s -> (s, Success x))
  (TM f) <*> (TM y) = TM (\c s ->
                           case (f c s) of
                              (s', Failure   ) -> (s', Failure)
                              (s', Error e   ) -> (s', Error e)
                              (s', Success f') ->
                                  case (y c s') of
                                    (s'', Failure   ) -> (s'', Failure)
                                    (s'', Error e'  ) -> (s'', Error e')
                                    (s'', Success y') -> (s'', Success (f' y')))


instance Monad TokenizerM where
  return = pure
  (TM x) >>= f = TM (\c s ->
                       case x c s of
                            (s', Failure   ) -> (s', Failure)
                            (s', Error e   ) -> (s', Error e)
                            (s', Success x') -> g c s'
                              where TM g = f x')

instance Alternative TokenizerM where
  empty = TM (\_ s -> (s, Failure))
  (<|>) (TM x) (TM y) = TM (\c s ->
                           case x c s of
                                (_, Failure   )  -> y c s
                                (s', Error e   ) -> (s', Error e)
                                (s', Success x') -> (s', Success x'))
  many (TM x) = TM (\c s ->
                    case x c s of
                       (_, Failure   )  -> (s, Success [])
                       (s', Error e   ) -> (s', Error e)
                       (s', Success x') -> mapsnd (fmap (x':)) (g c s')
                         where TM g = many (TM x))
  some x = (:) <$> x <*> many x

instance MonadPlus TokenizerM where
  mzero = empty
  mplus = (<|>)

instance MonadReader TokenizerConfig TokenizerM where
  ask = TM (\c s -> (s, Success c))
  local f (TM x) = TM (\c s -> x (f c) s)

instance MonadState TokenizerState TokenizerM where
  get = TM (\_ s -> (s, Success s))
  put x = TM (\_ _ -> (x, Success ()))

instance MonadError String TokenizerM where
  throwError e = TM (\_ s -> (s, Error e))
  catchError (TM x) f = TM (\c s -> case x c s of
                                      (_, Error e) -> let TM y = f e in y c s
                                      z            -> z)

-- | Tokenize some text using 'Syntax'.
tokenize :: TokenizerConfig -> Syntax -> Text -> Either String [SourceLine]
tokenize config syntax inp =
  case runTokenizerM action config initState of
       (_, Success ls) -> Right ls
       (_, Error e)    -> Left e
       (_, Failure)    -> Left "Could not tokenize code"
  where
    action = mapM tokenizeLine (zip (BS.lines (encodeUtf8 inp)) [1..])
    initState = startingState{ endline = Text.null inp
                             , contextStack =
                                   case lookupContext
                                         (sStartingContext syntax) syntax of
                                         Just c  -> ContextStack [c]
                                         Nothing -> ContextStack [] }


info :: String -> TokenizerM ()
info s = do
  tr <- asks traceOutput
  when tr $ trace s (return ())

infoContextStack :: TokenizerM ()
infoContextStack = do
  tr <- asks traceOutput
  when tr $ do
    ContextStack stack <- gets contextStack
    info $ "CONTEXT STACK " ++ show (map cName stack)

popContextStack :: TokenizerM ()
popContextStack = do
  ContextStack cs <- gets contextStack
  case cs of
       []     -> throwError "Empty context stack (the impossible happened)"
       -- programming error
       (_:[]) -> return ()
       (_:rest) -> do
         modify (\st -> st{ contextStack = ContextStack rest })
         currentContext >>= checkLineEnd
         infoContextStack

pushContextStack :: Context -> TokenizerM ()
pushContextStack cont = do
  modify (\st -> st{ contextStack =
                      ContextStack (cont : unContextStack (contextStack st)) } )
  -- not sure why we need this in pop but not here, but if we
  -- put it here we can get loops...
  -- checkLineEnd cont
  infoContextStack

currentContext :: TokenizerM Context
currentContext = do
  ContextStack cs <- gets contextStack
  case cs of
       []    -> throwError "Empty context stack" -- programming error
       (c:_) -> return c

doContextSwitch :: ContextSwitch -> TokenizerM ()
doContextSwitch Pop = popContextStack
doContextSwitch (Push (syn,c)) = do
  syntaxes <- asks syntaxMap
  case Map.lookup syn syntaxes >>= lookupContext c of
       Just con -> pushContextStack con
       Nothing  -> throwError $ "Unknown syntax or context: " ++ show (syn, c)

doContextSwitches :: [ContextSwitch] -> TokenizerM ()
doContextSwitches [] = return ()
doContextSwitches xs = do
  mapM_ doContextSwitch xs

lookupContext :: Text -> Syntax -> Maybe Context
lookupContext name syntax | Text.null name =
  if Text.null (sStartingContext syntax)
     then Nothing
     else lookupContext (sStartingContext syntax) syntax
lookupContext name syntax = Map.lookup name $ sContexts syntax

startingState :: TokenizerState
startingState =
  TokenizerState{ input = BS.empty
                , endline = True
                , prevChar = '\n'
                , contextStack = ContextStack []
                , captures = mempty
                , column = 0
                , lineContinuation = False
                , firstNonspaceColumn = Nothing
                , compiledRegexes = Map.empty
                }

tokenizeLine :: (ByteString, Int) -> TokenizerM [Token]
tokenizeLine (ln, linenum) = do
  modify $ \st -> st{ input = ln, endline = BS.null ln, prevChar = '\n' }
  cur <- currentContext
  lineCont <- gets lineContinuation
  if lineCont
     then modify $ \st -> st{ lineContinuation = False }
     else do
       modify $ \st -> st{ column = 0
                         , firstNonspaceColumn =
                              BS.findIndex (not . isSpace) ln }
       doContextSwitches (cLineBeginContext cur)
  if BS.null ln
     then doContextSwitches (cLineEmptyContext cur)
     else doContextSwitches (cLineBeginContext cur)
  ts <- normalizeHighlighting . catMaybes <$> many getToken
  eol <- gets endline
  if eol
     then do
       currentContext >>= checkLineEnd
       return ts
     else do  -- fail if we haven't consumed whole line
       col <- gets column
       throwError $ "Could not match anything at line " ++
         show linenum ++ " column " ++ show col

getToken :: TokenizerM (Maybe Token)
getToken = do
  inp <- gets input
  gets endline >>= guard . not
  context <- currentContext
  msum (map (\r -> tryRule r inp) (cRules context)) <|>
     case cFallthroughContext context of
           [] | cFallthrough context -> Nothing <$ doContextSwitches [Pop]
              | otherwise -> do
                  t <- normalChunk
                  let mbtok = Just (cAttribute context, t)
                  info $ "FALLTHROUGH " ++ show mbtok
                  return mbtok
           cs -> Nothing <$ doContextSwitches cs

takeChars :: Int -> TokenizerM Text
takeChars 0 = mzero
takeChars numchars = do
  inp <- gets input
  let (bs,rest) = UTF8.splitAt numchars inp
  guard $ not (BS.null bs)
  t <- decodeBS bs
  modify $ \st -> st{ input = rest,
                      endline = BS.null rest,
                      prevChar = Text.last t,
                      column = column st + numchars }
  return t

tryRule :: Rule -> ByteString -> TokenizerM (Maybe Token)
tryRule _    ""  = mzero
tryRule rule inp = do
  info $ "Trying rule " ++ show rule
  case rColumn rule of
       Nothing -> return ()
       Just n  -> gets column >>= guard . (== n)

  when (rFirstNonspace rule) $ do
    firstNonspace <- gets firstNonspaceColumn
    col <- gets column
    guard (firstNonspace == Just col)

  oldstate <- if rLookahead rule
                 then Just <$> get -- needed for lookahead rules
                 else return Nothing

  let attr = rAttribute rule
  mbtok <- case rMatcher rule of
                DetectChar c -> withAttr attr $ detectChar (rDynamic rule) c inp
                Detect2Chars c d -> withAttr attr $
                                      detect2Chars (rDynamic rule) c d inp
                AnyChar cs -> withAttr attr $ anyChar cs inp
                RangeDetect c d -> withAttr attr $ rangeDetect c d inp
                RegExpr re -> withAttr attr $ regExpr (rDynamic rule) re inp
                Int -> withAttr attr $ parseInt inp
                HlCOct -> withAttr attr $ parseOct inp
                HlCHex -> withAttr attr $ parseHex inp
                HlCStringChar -> withAttr attr $ parseCStringChar inp
                HlCChar -> withAttr attr $ parseCChar inp
                Float -> withAttr attr $ parseFloat inp
                Keyword kwattr kws -> withAttr attr $ keyword kwattr kws inp
                StringDetect s -> withAttr attr $
                                    stringDetect (rDynamic rule) (rCaseSensitive rule)
                                                 s inp
                WordDetect s -> withAttr attr $
                                    wordDetect (rCaseSensitive rule) s inp
                LineContinue -> withAttr attr $ lineContinue inp
                DetectSpaces -> withAttr attr $ detectSpaces inp
                DetectIdentifier -> withAttr attr $ detectIdentifier inp
                IncludeRules cname -> includeRules
                   (if rIncludeAttribute rule then Just attr else Nothing)
                   cname inp
  mbchildren <- do
    inp' <- gets input
    msum (map (\r -> tryRule r inp') (rChildren rule)) <|> return Nothing

  mbtok' <- case mbtok of
                 Nothing -> return Nothing
                 Just (tt, s)
                   | rLookahead rule -> do
                     (oldinput, oldendline, oldprevChar, oldColumn) <-
                         case oldstate of
                              Nothing -> throwError
                                    "oldstate not saved with lookahead rule"
                              Just st -> return
                                    (input st, endline st,
                                     prevChar st, column st)
                     modify $ \st -> st{ input = oldinput
                                       , endline = oldendline
                                       , prevChar = oldprevChar
                                       , column = oldColumn }
                     return Nothing
                   | otherwise -> do
                     case mbchildren of
                          Nothing -> return $ Just (tt, s)
                          Just (_, cresult) -> return $ Just (tt, s <> cresult)

  info $ takeWhile (/=' ') (show (rMatcher rule)) ++ " MATCHED " ++ show mbtok'
  doContextSwitches (rContextSwitch rule)
  return mbtok'

withAttr :: TokenType -> TokenizerM Text -> TokenizerM (Maybe Token)
withAttr tt p = do
  res <- p
  if Text.null res
     then return Nothing
     else return $ Just (tt, res)

wordDetect :: Bool -> Text -> ByteString -> TokenizerM Text
wordDetect caseSensitive s inp = do
  wordBoundary inp
  t <- decodeBS $ UTF8.take (Text.length s) inp
  -- we assume here that the case fold will not change length,
  -- which is safe for ASCII keywords and the like...
  guard $ if caseSensitive
             then s == t
             else mk s == mk t
  guard $ not (Text.null t)
  let c = Text.last t
  let rest = UTF8.drop (Text.length s) inp
  let d = case UTF8.uncons rest of
               Nothing    -> '\n'
               Just (x,_) -> x
  guard $ isWordBoundary c d
  takeChars (Text.length t)

stringDetect :: Bool -> Bool -> Text -> ByteString -> TokenizerM Text
stringDetect dynamic caseSensitive s inp = do
  s' <- if dynamic
        then do
          dynStr <- subDynamicText s
          info $ "Dynamic string: " ++ show dynStr
          return dynStr
        else return s
  t <- decodeBS $ UTF8.take (Text.length s') inp
  -- we assume here that the case fold will not change length,
  -- which is safe for ASCII keywords and the like...
  guard $ if caseSensitive
             then s' == t
             else mk s' == mk t
  takeChars (Text.length s')

subDynamicText :: Text -> TokenizerM Text
subDynamicText t = do
  let substitute x = case Text.uncons x of
        Just (c, rest) | isDigit c -> let capNum = ord c - ord '0'
                                      in (<> rest) <$> getCapture capNum
        _ -> return $ Text.cons '%' x
  case Text.split (== '%') t of
    []     -> return Text.empty
    x:rest -> (x <>) . Text.concat <$> mapM substitute rest

-- This assumes that nothing significant will happen
-- in the middle of a string of spaces or a string
-- of alphanumerics.  This seems true  for all normal
-- programming languages, and the optimization speeds
-- things up a lot, relative to just parsing one char.
normalChunk :: TokenizerM Text
normalChunk = do
  inp <- gets input
  case UTF8.uncons inp of
    Nothing -> mzero
    Just (c, _)
      | c == ' ' ->
        let bs = BS.takeWhile (==' ') inp
        in  takeChars (BS.length bs)
      | isAscii c && isAlphaNum c ->
        let (bs, _) = UTF8.span isAlphaNum inp
        in  takeChars (UTF8.length bs)
      | otherwise -> takeChars 1

includeRules :: Maybe TokenType -> ContextName -> ByteString
             -> TokenizerM (Maybe Token)
includeRules mbattr (syn, con) inp = do
  syntaxes <- asks syntaxMap
  case Map.lookup syn syntaxes >>= lookupContext con of
       Nothing  -> do
          cur <- currentContext
          throwError $ "IncludeRules in " ++ Text.unpack (cSyntax cur) ++
           " requires undefined context " ++
           Text.unpack con ++ "##" ++ Text.unpack syn
       Just c   -> do
         mbtok <- msum (map (\r -> tryRule r inp) (cRules c))
         return $ case (mbtok, mbattr) of
                    (Just (NormalTok, xs), Just attr) -> Just (attr, xs)
                    _                                 -> mbtok

checkLineEnd :: Context -> TokenizerM ()
checkLineEnd c = do
  if null (cLineEndContext c)
     then return ()
     else do
       eol <- gets endline
       info $ "checkLineEnd for " ++ show (cName c) ++ " eol = " ++ show eol ++ " cLineEndContext = " ++ show (cLineEndContext c)
       when eol $ do
         lineCont' <- gets lineContinuation
         unless lineCont' $
           doContextSwitches (cLineEndContext c)

detectChar :: Bool -> Char -> ByteString -> TokenizerM Text
detectChar dynamic c inp = do
  c' <- if dynamic && c >= '0' && c <= '9'
           then getDynamicChar c
           else return c
  case UTF8.uncons inp of
    Just (x,_) | x == c' -> takeChars 1
    _          -> mzero

getDynamicChar :: Char -> TokenizerM Char
getDynamicChar c = do
  let capNum = ord c - ord '0'
  res <- getCapture capNum
  case Text.uncons res of
       Nothing    -> mzero
       Just (d,_) -> return d

detect2Chars :: Bool -> Char -> Char -> ByteString -> TokenizerM Text
detect2Chars dynamic c d inp = do
  c' <- if dynamic && c >= '0' && c <= '9'
           then getDynamicChar c
           else return c
  d' <- if dynamic && d >= '0' && d <= '9'
           then getDynamicChar d
           else return d
  if (encodeUtf8 (Text.pack [c',d'])) `BS.isPrefixOf` inp
     then takeChars 2
     else mzero

rangeDetect :: Char -> Char -> ByteString -> TokenizerM Text
rangeDetect c d inp = do
  case UTF8.uncons inp of
    Just (x, rest)
      | x == c -> case UTF8.span (/= d) rest of
                       (in_t, out_t)
                         | BS.null out_t -> mzero
                         | otherwise -> do
                              t <- decodeBS in_t
                              takeChars (Text.length t + 2)
    _ -> mzero

-- NOTE: currently limited to ASCII
detectSpaces :: ByteString -> TokenizerM Text
detectSpaces inp = do
  case BS.span (\c -> isSpace c) inp of
       (t, _)
         | BS.null t -> mzero
         | otherwise -> takeChars (BS.length t)

-- NOTE: limited to ASCII as per kate documentation
detectIdentifier :: ByteString -> TokenizerM Text
detectIdentifier inp = do
  case BS.uncons inp of
    Just (c, t) | (isAscii c && isLetter c) || c == '_' ->
      takeChars $ 1 + maybe (BS.length t) id
                (BS.findIndex (\d -> isAscii d &&
                                     not (isAlphaNum d || d == '_')) t)
    _ -> mzero

lineContinue :: ByteString -> TokenizerM Text
lineContinue inp = do
  if inp == "\\"
     then do
       modify $ \st -> st{ lineContinuation = True }
       takeChars 1
     else mzero

anyChar :: Set.Set Char -> ByteString -> TokenizerM Text
anyChar cs inp = do
  case UTF8.uncons inp of
     Just (x, _) | x `Set.member` cs -> takeChars 1
     _           -> mzero

regExpr :: Bool -> RE -> ByteString -> TokenizerM Text
regExpr dynamic re inp = do
  -- return $! traceShowId $! (reStr, inp)
  let reStr = reString re
  when (BS.take 2 reStr == "\\b") $ wordBoundary inp
  compiledREs <- gets compiledRegexes
  regex <- case Map.lookup re compiledREs of
              Nothing -> do
                cre <- case compileRegex (reCaseSensitive re) reStr of
                         Right r  -> return r
                         Left e   -> throwError $
                           "Error compiling regex " ++
                            UTF8.toString reStr ++ ": " ++ e
                modify $ \st -> st{ compiledRegexes =
                      Map.insert re cre (compiledRegexes st) }
                return cre
              Just cre -> return cre
  regex' <- if dynamic
               then subDynamic regex
               else return regex
  case matchRegex regex' inp of
        Just (matchedBytes, capts) -> do
          unless (null capts) $
             modify $ \st -> st{ captures =
                                  IntMap.map (toSlice matchedBytes) capts }
          takeChars (UTF8.length matchedBytes)
        _ -> mzero

toSlice :: ByteString -> (Int, Int) -> ByteString
toSlice bs (off, len) = BS.take len $ BS.drop off bs

wordBoundary :: ByteString -> TokenizerM ()
wordBoundary inp = do
  case UTF8.uncons inp of
       Nothing -> return ()
       Just (d, _) -> do
         c <- gets prevChar
         guard $ isWordBoundary c d

isWordBoundary :: Char -> Char -> Bool
isWordBoundary c d = isWordChar c /= isWordChar d

decodeBS :: ByteString -> TokenizerM Text
decodeBS bs = case decodeUtf8' bs of
                    Left _ -> throwError ("ByteString " ++
                                show bs ++ "is not UTF8")
                    Right t -> return t

-- Substitute out %1, %2, etc. in regex string, escaping
-- appropriately..
subDynamic :: Regex -> TokenizerM Regex
subDynamic (MatchDynamic capNum) = do
  replacement <- getCapture capNum
  return $ mconcat $ map (MatchChar . (==)) $ Text.unpack replacement
subDynamic (MatchAlt r1 r2) =
  MatchAlt <$> subDynamic r1 <*> subDynamic r2
subDynamic (MatchConcat r1 r2) =
  MatchConcat <$> subDynamic r1 <*> subDynamic r2
subDynamic (MatchSome r) =
  MatchSome <$> subDynamic r
subDynamic (MatchCapture i r) =
  MatchCapture i <$> subDynamic r
subDynamic (AssertPositive dir r) =
  AssertPositive dir <$> subDynamic r
subDynamic (AssertNegative dir r) =
  AssertNegative dir <$> subDynamic r
subDynamic x = return x

getCapture :: Int -> TokenizerM Text
getCapture capnum = do
  capts <- gets captures
  case IntMap.lookup capnum capts of
     Nothing -> mzero
     Just x  -> decodeBS x

keyword :: KeywordAttr -> WordSet Text -> ByteString -> TokenizerM Text
keyword kwattr kws inp = do
  prev <- gets prevChar
  guard $ prev `Set.member` (keywordDelims kwattr)
  let (w,_) = UTF8.break (`Set.member` (keywordDelims kwattr)) inp
  guard $ not (BS.null w)
  w' <- decodeBS w
  let numchars = Text.length w'
  if w' `inWordSet` kws
     then takeChars numchars
     else mzero

normalizeHighlighting :: [Token] -> [Token]
normalizeHighlighting [] = []
normalizeHighlighting ((t,x):xs)
  | Text.null x = normalizeHighlighting xs
  | otherwise =
    (t, Text.concat (x : map snd matches)) : normalizeHighlighting rest
    where (matches, rest) = span (\(z,_) -> z == t) xs

parseCStringChar :: ByteString -> TokenizerM Text
parseCStringChar inp = do
  case A.parseOnly (A.match pCStringChar) inp of
       Left _      -> mzero
       Right (r,_) -> takeChars (BS.length r) -- assumes ascii

pCStringChar :: A.Parser ()
pCStringChar = do
  _ <- A.char '\\'
  next <- A.anyChar
  case next of
       c | c == 'x' || c == 'X' -> () <$ A.takeWhile1 (A.inClass "0-9a-fA-F")
         | c == '0' -> () <$ A.takeWhile (A.inClass "0-7")
         | A.inClass "abefnrtv\"'?\\" c -> return ()
         | otherwise -> mzero

parseCChar :: ByteString -> TokenizerM Text
parseCChar inp = do
  case A.parseOnly (A.match pCChar) inp of
       Left _      -> mzero
       Right (r,_) -> takeChars (BS.length r) -- assumes ascii

pCChar :: A.Parser ()
pCChar = do
  () <$ A.char '\''
  pCStringChar <|> () <$ A.satisfy (\c -> c /= '\'' && c /= '\\')
  () <$ A.char '\''

parseInt :: ByteString -> TokenizerM Text
parseInt inp = do
  wordBoundary inp
  case A.parseOnly (A.match (pHex <|> pOct <|> pDec)) inp of
       Left _      -> mzero
       Right (r,_) -> takeChars (BS.length r) -- assumes ascii

pDec :: A.Parser ()
pDec = do
  mbMinus
  _ <- A.takeWhile1 (A.inClass "0-9")
  guardWordBoundary

parseOct :: ByteString -> TokenizerM Text
parseOct inp = do
  wordBoundary inp
  case A.parseOnly (A.match pHex) inp of
       Left _      -> mzero
       Right (r,_) -> takeChars (BS.length r) -- assumes ascii

pOct :: A.Parser ()
pOct = do
  mbMinus
  _ <- A.char '0'
  _ <- A.satisfy (A.inClass "Oo")
  _ <- A.takeWhile1 (A.inClass "0-7")
  guardWordBoundary

parseHex :: ByteString -> TokenizerM Text
parseHex inp = do
  wordBoundary inp
  case A.parseOnly (A.match pHex) inp of
       Left _      -> mzero
       Right (r,_) -> takeChars (BS.length r) -- assumes ascii

pHex :: A.Parser ()
pHex = do
  mbMinus
  _ <- A.char '0'
  _ <- A.satisfy (A.inClass "Xx")
  _ <- A.takeWhile1 (A.inClass "0-9a-fA-F")
  guardWordBoundary

guardWordBoundary :: A.Parser ()
guardWordBoundary = do
  mbw <- A.peekChar
  case mbw of
       Just c  ->  guard $ isWordBoundary '0' c
       Nothing -> return ()

mbMinus :: A.Parser ()
mbMinus = (() <$ A.char '-') <|> return ()

mbPlusMinus :: A.Parser ()
mbPlusMinus = () <$ A.satisfy (A.inClass "+-") <|> return ()

parseFloat :: ByteString -> TokenizerM Text
parseFloat inp = do
  wordBoundary inp
  case A.parseOnly (A.match pFloat) inp of
       Left _      -> mzero
       Right (r,_) -> takeChars (BS.length r)  -- assumes all ascii
  where pFloat :: A.Parser ()
        pFloat = do
          let digits = A.takeWhile1 (A.inClass "0-9")
          mbPlusMinus
          before <- A.option False $ True <$ digits
          dot <- A.option False $ True <$ A.satisfy (A.inClass ".")
          after <- A.option False $ True <$ digits
          e <- A.option False $ True <$ (A.satisfy (A.inClass "Ee") >>
                                         mbPlusMinus >> digits)
          mbnext <- A.peekChar
          case mbnext of
               Nothing -> return ()
               Just c  -> guard (not $ A.inClass "." c)
          guard $ (before && not dot && e)     -- 5e2
               || (before && dot && (after || not e)) -- 5.2e2 or 5.2 or 5.
               || (not before && dot && after) -- .23 or .23e2

