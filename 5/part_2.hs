import System.Environment
import Data.Map (Map, insert, empty, lookup, elems, keys)
import Data.Maybe (fromMaybe)
import Data.Set (Set, insert, empty, union, toList, fromList, difference)
import GHC.Base (divInt)
import Debug.Trace (trace, traceShow)

-- less naive:
-- reverse the rules and put them in a multimap
-- scan left to right
-- map each page to the ones that must come before it
-- add those to a banlist
-- if anything on the banlist is found, reject, else accept

middlePage :: [String] -> Integer
middlePage x = read (x !! (length x `divInt` 2))

isValid :: Map String (Set String) -> Set String -> [String] -> Bool
isValid _ _ [] = True
isValid rulesMap banlist update
  | head update `elem` banlist = False
  | otherwise = isValid rulesMap newBanlist (tail update)
  where newBanlist = banlist `union` addToBanlist
        addToBanlist = fromMaybe Data.Set.empty (Data.Map.lookup (head update) rulesMap)

buildRulesMap :: [String] -> Map String (Set String)
buildRulesMap [] = Data.Map.empty
buildRulesMap (x:xs) = Data.Map.insert k (Data.Set.insert v priorVal) priorMap
  where [v, k] = split '|' x
        priorMap = buildRulesMap xs
        priorVal = fromMaybe Data.Set.empty (Data.Map.lookup k priorMap)

split :: Eq a => a -> [a] -> [[a]]
split delimiter [] = []
split delimiter x = as : split delimiter (drop 1 bs)
  where (as, bs) = span (/= delimiter) x

buildUrpdate :: [(String, String)] -> [String]
buildUrpdate [] = []
buildUrpdate rules = do
    let _ = trace "foo"
    let _ = trace ("rules: " ++ show rules)
    let _ = trace ("pres: " ++ show pres)
    let _ = trace ("posts: " ++ show posts)
    traceShow nexts (toList nexts ++ buildUrpdate remainingRules)
    where pres = traceShow (Data.Set.fromList $ map fst rules) (Data.Set.fromList $ map fst rules)
          posts = traceShow (Data.Set.fromList $ map snd rules) (Data.Set.fromList $ map snd rules)
          nexts = traceShow (Data.Set.difference pres posts) (Data.Set.difference pres posts)
          remainingRules = traceShow (filter (not . (`elem` nexts) . fst) rules) (filter (not . (`elem` nexts) . fst) rules)

parseRule :: String -> (String, String)
parseRule rule = (k, v)
    where [k, v] = split '|' rule

main :: IO ()
main = do
    args <- getArgs
    contents <- lines <$> readFile (head args)
    let [rules, updates] = split "" contents
    let _ = trace "bar"
    print $ (buildUrpdate . map parseRule) rules
    let rulesMap = buildRulesMap rules
    let splitUpdates = map (split ',') updates
    let validUpdates = filter (isValid rulesMap Data.Set.empty) splitUpdates
    print $ sum $ map middlePage validUpdates
