module WC3PL.Maps (WC3Map,
             getFeatures,
             buildMap) where
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.IO (IOArray)

type Grid = IOArray Int [WC3Feature]
type Location = (Int, Int)
type Dimensions = (Int, Int)

data WC3Map = WC3Map Dimensions Grid

instance Show WC3Map where
  show (WC3Map (x,y) grid) = "WC3Map (" ++ show x ++ ", " ++ show y ++ ")"

data WC3Feature = Goldmine
                | Tree
                | Townhall
                | Barracks
                | Peasant
                | Footman
                | Tile Int
                  deriving Show

getFeatures :: WC3Map -> Location -> IO [WC3Feature]
getFeatures map@(WC3Map _ grid) loc = readArray grid (translate map loc)

translate :: WC3Map -> Location -> Int
translate (WC3Map (maxX,maxY) _) (x,y) | 1 <= x && x <= maxX && 1 <= y && y <= maxY = (maxX-1)*x + y
translate (WC3Map (maxX,maxY) _) (x,y) | otherwise = error $ "Tried to dereference location: (" ++ (show x) ++ ", " ++ (show y) ++ ") in map with dimensions: (" ++ (show maxX) ++ ", " ++ (show maxY) ++ ")"

buildMap :: String -> IO WC3Map
buildMap inp = do
  let rows = lines inp
      maxY = length rows
      maxX = length (head rows)
      rowLens = map length rows
    in
   if not $ all (== maxX) rowLens
   then error "Map is not rectangular, some rows are longer than others"
   else populateMap (maxX, maxY) rows

populateMap :: Dimensions -> [String] -> IO WC3Map
populateMap (x, y) rows = do
  grid <- newListArray (1, (x*y)) (map ((\x -> [x]) . charToFeature) (concat rows))
  return $ WC3Map (x,y) grid

charToFeature :: Char -> WC3Feature
charToFeature '.' = Tile 0
charToFeature 'w' = Tree
charToFeature 'g' = Goldmine
charToFeature 'h' = Townhall
charToFeature a = error $ "Did not recognize map character: " ++ show a