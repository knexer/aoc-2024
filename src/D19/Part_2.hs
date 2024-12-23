{-# OPTIONS_GHC -Wno-type-defaults #-}

module D19.Part_2 where

import Data.Foldable (Foldable (toList))
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Utils (split)

type Pattern = [Char]

data PatternTrie = PatternTrie {terminus :: Bool, suffixes :: (Map.Map Char PatternTrie)} deriving (Show)

parse :: String -> (PatternTrie, [Pattern])
parse contents = (buildTrie towels, goals)
  where
    (towelsPart, goalsPart) = span (/= "") (lines contents)
    goals :: [Pattern] = drop 1 goalsPart
    towels :: [Pattern] = (map (filter (/= ' ')) . split ',') (towelsPart !! 0)

buildTrie :: [Pattern] -> PatternTrie
buildTrie = foldr add emptyTrie

emptyTrie :: PatternTrie
emptyTrie = PatternTrie {terminus = False, suffixes = Map.empty}

add :: Pattern -> PatternTrie -> PatternTrie
add [] trie = trie {terminus = True}
add (x : xs) trie = trie {suffixes = Map.insert x (add xs prevSubTrie) (suffixes trie)}
  where
    prevSubTrie :: PatternTrie = Map.findWithDefault emptyTrie x (suffixes trie)

prefixes :: Pattern -> PatternTrie -> [Int]
prefixes [] trie = if terminus trie then [0] else []
prefixes (x : xs) trie = self ++ recurse
  where
    self = if terminus trie then [0] else []
    subTrie = Map.lookup x (suffixes trie)
    recurse = map (+ 1) . fromMaybe [] $ fmap (prefixes xs) subTrie

-- strategy
-- starting with a pattern, find all indices we can immediately make
-- then, from each of those indices, recurse
-- BUT dedupe the recursion so it isn't exponential

canReachEnd :: PatternTrie -> [(Int, Int)] -> Pattern -> Int
canReachEnd _ [] _ = 0
canReachEnd trie (x : xs) pattern
  | fst x == length pattern = snd x
  | otherwise = canReachEnd trie nextReachable pattern
  where
    reachableFromX = map ((,snd x) . (+ fst x)) $ prefixes (drop (fst x) pattern) trie
    -- combine xs and reachableFromX
    -- if same fst, add snd
    nextReachableMap = Map.unionWith (+) (Map.fromList xs) (Map.fromList reachableFromX)
    nextReachable = (sort . Map.toList) nextReachableMap

main :: String -> IO ()
main contents = do
  let (trie, goals) = parse contents
  let canReach = map (canReachEnd trie [(0, 1)]) goals
  print canReach
  print $ (sum . map (canReachEnd trie [(0, 1)])) goals
