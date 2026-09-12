{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
module Regex.KDE.Match
 ( matchRegex
 ) where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as U
import Data.Char (toLower)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Bits (shiftL, (.|.))
import Data.Word (Word8)
import Regex.KDE.Regex
import qualified Data.IntMap.Strict as M
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

-- Note that all matches are from the beginning of the string.
-- The ^ anchor is implicit at the beginning of the regex.

-- To reproduce PCRE's leftmost-first (backtracking) semantics in a
-- set-based matcher, every match carries a path recording the choices
-- made to reach it: 0 for taking the left branch of an alternation or
-- continuing a greedy repetition, 1 for the right branch or ending
-- the repetition (for lazy repetitions the loop codes are reversed).
-- Comparing paths lexicographically gives the order in which a
-- backtracking matcher like PCRE would find the matches, so the
-- preferred match is always the one with the smallest path.

-- A sequence of binary choices, packed into an Integer (most recent
-- choice in the least significant bit) together with its length.
-- Compared lexicographically as a bit sequence, with a proper prefix
-- ordered before its extensions.
data Path = Path !Integer !Int
  deriving (Show, Eq)

instance Ord Path where
  compare (Path i1 l1) (Path i2 l2) =
    compare (i1 `shiftL` max 0 (l2 - l1)) (i2 `shiftL` max 0 (l1 - l2))
      <> compare l1 l2

emptyPath :: Path
emptyPath = Path 0 0

pathSnoc :: Path -> Word8 -> Path
pathSnoc (Path i l) b = Path ((i `shiftL` 1) .|. fromIntegral b) (l + 1)

data Match =
   Match { matchBytes    :: !ByteString
         , matchOffset   :: !Int
         , matchCaptures :: !(M.IntMap (Int, Int))
                                  -- starting offset, length in bytes
         , matchPath     :: !Path
         } deriving (Show, Eq)

-- preferred matches are <=; the path (priority) is decisive, and the
-- other comparisons only make the order total:
instance Ord Match where
  compare m1 m2 =
    compare (matchPath m1) (matchPath m2) <>
    compare (matchOffset m1) (matchOffset m2) <>
    compare (matchCaptures m1) (matchCaptures m2)

-- the state of a match, disregarding its priority
stateKey :: Match -> (Int, M.IntMap (Int, Int))
stateKey m = (matchOffset m, matchCaptures m)

-- append a choice to the path of every match
addChoice :: Word8 -> Set Match -> Set Match
addChoice !b = Set.map (\m -> m{ matchPath = pathSnoc (matchPath m) b })

-- Discard any match whose state coincides with that of a preferred
-- (smaller-path) match: their futures are identical, and a
-- backtracking matcher would explore the preferred one first.
dedup :: Set Match -> Set Match
dedup = snd . Set.foldl' step (Set.empty, Set.empty)
 where
  step (!seen, !out) m
    | stateKey m `Set.member` seen = (seen, out)
    | otherwise = (Set.insert (stateKey m) seen, Set.insert m out)

mapMatching :: (Match -> Match) -> Set Match -> Set Match
mapMatching f = Set.filter ((>= 0) . matchOffset) . Set.map f

-- we take the n best matches to avoid pathological slowdown
sizeLimit :: Int
sizeLimit = 2000

-- prune matches if it gets out of hand, keeping preferred matches
prune :: Set Match -> Set Match
prune ms = if Set.size ms > sizeLimit
              then Set.take sizeLimit ms
              else ms

-- first argument: the set of subroutine calls (group number, offset)
-- currently being evaluated -- used to prevent infinite recursion --
-- and a map of capturing groups, needed for Subroutine.
exec :: (Set (Int, Int), M.IntMap Regex)
     -> Direction -> Regex -> Set Match -> Set Match
exec _ _ MatchNull = id
exec cgs dir (Lazy (MatchSome re)) = someLoop cgs dir re 1 0
exec cgs dir (Lazy re) = -- Lazy is only applied to MatchSome (see Compile)
  exec cgs dir re
exec cgs dir (Possessive re) =
  -- commit to the first match (in backtracking order) of re; its
  -- internal choices are forgotten, so the path is reset:
  Set.foldl'
    (\s m -> case Set.lookupMin (exec cgs dir re (Set.singleton m)) of
               Nothing -> s
               Just m' -> Set.insert m'{ matchPath = matchPath m } s)
    mempty
exec cgs dir (MatchDynamic n) = -- if this hasn't been replaced, match literal
  exec cgs dir (MatchChar (== '%') <>
            mconcat (map (\c -> MatchChar (== c)) (show n)))
exec _ _ AssertEnd = Set.filter (\m -> matchOffset m == B.length (matchBytes m))
exec _ _ AssertBeginning = Set.filter (\m -> matchOffset m == 0)
exec cgs _ (AssertPositive dir regex) =
  -- assertions are atomic: only the captures of the first match (in
  -- backtracking order) of the assertion are kept, as in PCRE:
  Set.foldl'
    (\s m -> case Set.lookupMin (exec cgs dir regex (Set.singleton m)) of
               Nothing -> s
               Just m' -> Set.insert
                            m'{ matchBytes = matchBytes m
                              , matchOffset = matchOffset m
                              , matchPath = matchPath m } s)
    mempty
exec cgs _ (AssertNegative dir regex) =
  Set.filter (\m -> null (exec cgs dir regex (Set.singleton m)))
exec _ _ AssertWordBoundary = Set.filter atWordBoundary
exec _ Forward MatchAnyChar = mapMatching $ \m ->
  case U.decode (B.drop (matchOffset m) (matchBytes m)) of
    Nothing -> m{ matchOffset = - 1}
    Just (_,n) -> m{ matchOffset = matchOffset m + n }
exec _ Backward MatchAnyChar = mapMatching $ \m ->
  case lastCharOffset (matchBytes m) (matchOffset m) of
    Nothing  -> m{ matchOffset = -1 }
    Just off -> m{ matchOffset = off }
exec _ Forward (MatchChar f) = mapMatching $ \m ->
  case U.decode (B.drop (matchOffset m) (matchBytes m)) of
    Just (c,n) | f c -> m{ matchOffset = matchOffset m + n }
    _ -> m{ matchOffset = -1 }
exec _ Backward (MatchChar f) = mapMatching $ \m ->
  case lastCharOffset (matchBytes m) (matchOffset m) of
    Nothing  -> m{ matchOffset = -1 }
    Just off ->
      case U.decode (B.drop off (matchBytes m)) of
        Just (c,_) | f c -> m{ matchOffset = off }
        _                -> m{ matchOffset = -1 }
exec cgs dir (MatchConcat (MatchConcat r1 r2) r3) =
  exec cgs dir (MatchConcat r1 (MatchConcat r2 r3))
exec cgs Forward (MatchConcat r1 r2) =
  \ms ->
    let ms1 = exec cgs Forward r1 ms
     in if Set.null ms1
           then ms1
           else exec cgs Forward r2 (prune ms1)
exec cgs Backward (MatchConcat r1 r2) =
  exec cgs Backward r1 . exec cgs Backward r2
exec cgs dir (MatchAlt r1 r2) = \ms ->
  dedup $ exec cgs dir r1 (addChoice 0 ms) <> exec cgs dir r2 (addChoice 1 ms)
exec cgs dir (MatchSome re) = someLoop cgs dir re 0 1
exec cgs dir (MatchCapture i re) =
  Set.foldr Set.union Set.empty .
   Set.map (\m ->
     Set.map (captureDifference m) (exec cgs dir re (Set.singleton m)))
 where
    captureDifference m m' =
      let len = matchOffset m' - matchOffset m
      in  m'{ matchCaptures = M.insert i (matchOffset m, len)
                                  (matchCaptures m') }
exec _ dir (MatchCaptured n caseSensitive) = mapMatching matchCaptured
 where
   matchCaptured m =
     case M.lookup n (matchCaptures m) of
       Just (offset, len) ->
              let capture = B.take len $ B.drop offset $ matchBytes m
              in  case dir of
                     Forward
                       | caseSensitive
                       , B.isPrefixOf capture
                           (B.drop (matchOffset m) (matchBytes m))
                        -> m{ matchOffset = matchOffset m + B.length capture }
                       | not caseSensitive
                       , Just len' <- ciPrefixLength (U.toString capture)
                             (B.drop (matchOffset m) (matchBytes m))
                        -> m{ matchOffset = matchOffset m + len' }
                     Backward
                       | caseSensitive
                       , B.isSuffixOf capture
                           (B.take (matchOffset m) (matchBytes m))
                        -> m{ matchOffset = matchOffset m - B.length capture }
                       | not caseSensitive
                       , Just off' <- ciSuffixOffset
                             (reverse (U.toString capture))
                             (matchBytes m) (matchOffset m)
                        -> m{ matchOffset = off' }
                     _  -> m{ matchOffset = -1 }
       Nothing -> m{ matchOffset = -1 }
exec (active, cgs) dir (Subroutine i) =
  case M.lookup i cgs of
    Nothing -> id  -- ignore references to nonexistent groups
    Just re' -> \ms ->
      -- A subroutine that calls itself again without having consumed
      -- any input can never make progress: block re-entry at the same
      -- offset so that zero-progress recursion (e.g. `x|(?R)`) fails
      -- instead of looping forever.
      dedup $ Set.unions
        [ exec (Set.insert (i, matchOffset m) active, cgs) dir re'
            (Set.singleton m)
        | m <- Set.toList ms
        , (i, matchOffset m) `Set.notMember` active ]

-- Match one or more repetitions of a regex.  contB is the path code
-- appended when continuing with another repetition, stopB the one
-- appended when stopping: 0/1 for greedy, 1/0 for lazy repetitions,
-- so that the paths order the results the way a backtracking matcher
-- would find them.
someLoop :: (Set (Int, Int), M.IntMap Regex) -> Direction -> Regex
         -> Word8 -> Word8 -> Set Match -> Set Match
someLoop cgs dir re !contB !stopB = \ms0 ->
  let ms1 = dedup $ exec cgs dir re ms0  -- first, obligatory repetition
   in go (Set.map stateKey ms1) ms1
 where
  go !seen ms
    | Set.null ms = Set.empty
    | otherwise =
        let ms' = dedup $ prune $ exec cgs dir re (addChoice contB ms)
            -- Drop matches that revisit an already-seen state: they
            -- have no new futures, and this guarantees termination
            -- when an iteration can match the empty string.
            new = Set.filter (\m -> stateKey m `Set.notMember` seen) ms'
         in addChoice stopB ms <> go (seen <> Set.map stateKey new) new

atWordBoundary :: Match -> Bool
atWordBoundary m =
  case lastCharOffset (matchBytes m) (matchOffset m) of
    Nothing  -> True
    Just off ->
      case U.toString (B.drop off (matchBytes m)) of
        (cur:next:_) -> isWordChar cur /= isWordChar next
        _ -> True

-- If the characters of the first argument match the beginning of the
-- bytestring case-insensitively, return the length in bytes of the
-- matching prefix.
ciPrefixLength :: String -> ByteString -> Maybe Int
ciPrefixLength [] _ = Just 0
ciPrefixLength (c:cs) bs =
  case U.decode bs of
    Just (d, n) | toLower d == toLower c ->
      (n +) <$> ciPrefixLength cs (B.drop n bs)
    _ -> Nothing

-- If the characters of the first argument (reversed) match the
-- characters just before the given offset case-insensitively, return
-- the offset at which the match begins.
ciSuffixOffset :: String -> ByteString -> Int -> Maybe Int
ciSuffixOffset [] _ off = Just off
ciSuffixOffset (c:cs) bs off =
  case lastCharOffset bs off of
    Just off' | Just (d, _) <- U.decode (B.drop off' bs)
              , toLower d == toLower c -> ciSuffixOffset cs bs off'
    _ -> Nothing

-- Return the offset of the start of the (UTF-8 encoded) character
-- that ends at (i.e., whose last byte is just before) offset n.
lastCharOffset :: ByteString -> Int -> Maybe Int
lastCharOffset _ 0 = Nothing
lastCharOffset bs n = go (n - 1)
 where
  go !k
    | k <= 0 = Just 0
    | isContinuationByte (B.index bs k) = go (k - 1)
    | otherwise = Just k
  isContinuationByte w = w >= 0b10000000 && w < 0b11000000

-- | Match a Regex against a (presumed UTF-8 encoded) ByteString,
-- returning the matched text and a map of (offset, size)
-- pairs for captures.  Note that all matches are from the
-- beginning of the string (a @^@ anchor is implicit).  As in
-- PCRE, the match returned is the first one that a backtracking
-- matcher would find (leftmost alternatives are preferred), which
-- is not necessarily the longest.  Note also that to avoid
-- pathological performance in certain cases, the matcher is limited
-- to considering 2000 possible matches at a time; when that
-- threshold is reached, it discards lower-priority matches.  Hence
-- certain regexes may incorrectly fail to match: e.g. @a*a{3000}$@
-- on a string of 3000 @a@s.
matchRegex :: Regex
           -> ByteString
           -> Maybe (ByteString, M.IntMap (Int, Int))
matchRegex re bs =
  let capturingGroups = extractCapturingGroups re
  in  toResult <$> Set.lookupMin
               (exec (Set.empty, capturingGroups) Forward re
                  (Set.singleton (Match bs 0 M.empty emptyPath)))
 where
   toResult m = (B.take (matchOffset m) (matchBytes m), (matchCaptures m))

extractCapturingGroups :: Regex -> M.IntMap Regex
extractCapturingGroups regex = M.insert 0 regex (go regex)
 where
  -- Note: left-biased union means that with (?|...), which reuses
  -- group numbers, the first alternative's group wins.
  go (MatchSome re) = go re
  go (MatchAlt re1 re2) = go re1 <> go re2
  go (MatchConcat re1 re2) = go re1 <> go re2
  go (MatchCapture i re) = M.insert i re (go re)
  go (AssertPositive _ re) = go re
  go (AssertNegative _ re) = go re
  go (Possessive re) = go re
  go (Lazy re) = go re
  go _ = mempty
