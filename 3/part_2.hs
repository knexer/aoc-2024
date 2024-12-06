import System.Environment
import GHC.Settings.Utils (maybeRead)
import Debug.Trace (traceShowId, traceId, trace)
import Data.Text (splitOn, pack, unpack)

maybeMult2 :: String -> String -> Maybe Int
maybeMult2 left right =
  if ')' `elem` right then product else Nothing
  where
    product = (*) <$> maybeRead left <*> rightNum
    rightNum = maybeRead (takeWhile (/= ')') right)

maybeMult :: String -> Maybe Int
maybeMult str = do
  let stripped = maybeStrip "ul(" str
  -- traceShowId str (??? why won't think compile ???)
  -- traceShowId stripped
  let splitted = split ',' <$> stripped
  -- traceShowId splitted
  case splitted of
    Just (x:y:ys) -> maybeMult2 x y
    _ -> Nothing

maybeStrip :: String -> String -> Maybe String
maybeStrip [] string = Just string
maybeStrip prefix string
  | head prefix == head string = maybeStrip (tail prefix) (tail string)
  | otherwise = Nothing

split :: Char -> String -> [String]
split delimiter [] = []
split delimiter x = as : split delimiter (drop 1 bs)
  where (as, bs) = span (/= delimiter) x

getDos :: String -> [String]
getDos str = map unpack doUntilDont
  where dos = splitOn (pack "do()") (pack str) -- each of these starts enabled
        doUntilDont = map (head . splitOn (pack "don't()")) dos -- the tail of each split is disabled

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let foo = splitOn (pack "do()") (pack contents)
    -- split by "mul("
    -- read up to ","
    -- read up to ")"
    -- putStr $ unlines $ zipWith (curry show) (maybeMult <$> split 'm' contents) (map ("m" ++) (split 'm' contents))
    print $ sum $ fmap sum (maybeMult <$> split 'm' (concat (getDos contents)))
