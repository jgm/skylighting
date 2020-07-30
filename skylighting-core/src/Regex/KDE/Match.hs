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
import qualified Data.Set as Set
import Data.Set (Set)
import Regex.KDE.Regex
import qualified Data.IntMap.Strict as M
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

-- Note that all matches are from the beginning of the string.
-- The ^ anchor is implicit at the beginning of the regex.

data Match =
   Match { matchBytes    :: !ByteString
         , matchOffset   :: !Int
         , matchCaptures :: !(M.IntMap (Int, Int))
                       -- starting offset, length in bytes
         } deriving (Show, Eq)

-- longer matches are <=
instance Ord Match where
  m1 <= m2 = matchOffset m1 >= matchOffset m2 &&
             matchCaptures m1 >= matchCaptures m2

mapMatching :: (Match -> Match) -> Set Match -> Set Match
mapMatching f = Set.filter ((>= 0) . matchOffset) . Set.map f

-- we take the n longest matches to avoid pathological slowdown
sizeLimit :: Int
sizeLimit = 2000

-- prune matches if it gets out of hand
prune :: Set Match -> Set Match
prune ms = if Set.size ms > sizeLimit
              then Set.take sizeLimit ms
              else ms

exec :: Direction -> Regex -> Set Match -> Set Match
exec _ MatchNull = id
exec dir (MatchDynamic n) = -- if this hasn't been replaced, match literal
  exec dir (MatchChar (== '%') <>
            mconcat (map (\c -> MatchChar (== c)) (show n)))
exec _ AssertEnd = Set.filter (\m -> matchOffset m == B.length (matchBytes m))
exec _ AssertBeginning = Set.filter (\m -> matchOffset m == 0)
exec _ (AssertPositive dir regex) =
  Set.filter (\m -> not (null (exec dir regex (Set.singleton m))))
exec _ (AssertNegative dir regex) =
  Set.filter (\m -> null (exec dir regex (Set.singleton m)))
exec _ AssertWordBoundary = Set.filter atWordBoundary
exec Forward MatchAnyChar = mapMatching $ \m ->
  case U.decode (B.drop (matchOffset m) (matchBytes m)) of
    Nothing -> m{ matchOffset = - 1}
    Just (_,n) -> m{ matchOffset = matchOffset m + n }
exec Backward MatchAnyChar = mapMatching $ \m ->
  case lastCharOffset (matchBytes m) (matchOffset m) of
    Nothing  -> m{ matchOffset = -1 }
    Just off -> m{ matchOffset = off }
exec Forward (MatchChar f) = mapMatching $ \m ->
  case U.decode (B.drop (matchOffset m) (matchBytes m)) of
    Just (c,n) | f c -> m{ matchOffset = matchOffset m + n }
    _ -> m{ matchOffset = -1 }
exec Backward (MatchChar f) = mapMatching $ \m ->
  case lastCharOffset (matchBytes m) (matchOffset m) of
    Nothing  -> m{ matchOffset = -1 }
    Just off ->
      case U.decode (B.drop off (matchBytes m)) of
        Just (c,_) | f c -> m{ matchOffset = off }
        _                -> m{ matchOffset = -1 }
exec dir (MatchConcat (MatchConcat r1 r2) r3) =
  exec dir (MatchConcat r1 (MatchConcat r2 r3))
exec Forward (MatchConcat r1 r2) = -- TODO longest match first
  exec Forward r2 . prune . exec Forward r1
exec Backward (MatchConcat r1 r2) = exec Backward r1 . exec Backward r2
exec dir (MatchAlt r1 r2) = \ms -> exec dir r1 ms <> exec dir r2 ms
exec dir (MatchSome re) = go
 where
  go ms = case exec dir re ms of
            ms' | Set.null ms' -> Set.empty
                | ms' == ms    -> ms
                | otherwise    -> let ms'' = prune ms'
                                   in ms'' <> go ms''
exec dir (MatchCapture i re) =
  Set.foldr Set.union Set.empty .
   Set.map (\m ->
     Set.map (captureDifference m) (exec dir re (Set.singleton m)))
 where
    captureDifference m m' =
      let len = matchOffset m' - matchOffset m
      in  m'{ matchCaptures = M.insert i (matchOffset m, len)
                                  (matchCaptures m') }
exec dir (MatchCaptured n) = mapMatching matchCaptured
 where
   matchCaptured m =
     case M.lookup n (matchCaptures m) of
       Just (offset, len) ->
              let capture = B.take len $ B.drop offset $ matchBytes m
              in  case dir of
                     Forward | B.isPrefixOf capture
                                 (B.drop (matchOffset m) (matchBytes m))
                        -> m{ matchOffset = matchOffset m + B.length capture }
                     Backward | B.isSuffixOf capture
                                 (B.take (matchOffset m) (matchBytes m))
                        -> m{ matchOffset = matchOffset m - B.length capture }
                     _  -> m{ matchOffset = -1 }
       Nothing -> m{ matchOffset = -1 }


atWordBoundary :: Match -> Bool
atWordBoundary m =
  case matchOffset m of
    0 -> True
    n | n == B.length (matchBytes m) -> True
      | otherwise ->
           case lastCharOffset (matchBytes m) (matchOffset m) of
             Nothing  -> True
             Just off ->
               case U.toString (B.drop (off - 1) (matchBytes m)) of
                 (prev:cur:next:_) ->
                   (isWordChar cur /= isWordChar next) ||
                   (isWordChar cur /= isWordChar prev)
                 _ -> True

lastCharOffset :: ByteString -> Int -> Maybe Int
lastCharOffset _ 0 = Nothing
lastCharOffset _ 1 = Nothing
lastCharOffset bs n =
  case B.index bs (n - 2) of
    w | w <  0b10000000 -> Just (n - 1)
      | w >= 0b11000000 -> Just (n - 1)
      | otherwise -> lastCharOffset bs (n - 1)

-- | Match a Regex against a (presumed UTF-8 encoded) ByteString,
-- returning the matched text and a map of (offset, size)
-- pairs for captures.  Note that all matches are from the
-- beginning of the string (a @^@ anchor is implicit).  Note
-- also that to avoid pathological performance in certain cases,
-- the matcher is limited to considering 2000 possible matches
-- at a time; when that threshold is reached, it discards
-- smaller matches.  Hence certain regexes may incorrectly fail to
-- match: e.g. @a*a{3000}$@ on a string of 3000 @a@s.
matchRegex :: Regex
           -> ByteString
           -> Maybe (ByteString, M.IntMap (Int, Int))
matchRegex re bs =
  toResult <$> Set.lookupMin
               (exec Forward re (Set.singleton (Match bs 0 M.empty)))
 where
   toResult m = (B.take (matchOffset m) (matchBytes m), (matchCaptures m))

