module Utils where

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delimiter x = as : split delimiter (drop 1 bs)
  where
    (as, bs) = span (/= delimiter) x

coordinates :: [[(Int, Int)]]
coordinates = map rowNumberToIds [0..]
  where
    rowNumberToIds rowNumber = map (,rowNumber) [0..]

zipWithCoords :: [[a]] -> [[((Int, Int), a)]]
zipWithCoords = zipWith zip coordinates

    -- let startingMap = (locFromChar <$>) <$> contentsWithIndex -- TODO should be unique integers for each