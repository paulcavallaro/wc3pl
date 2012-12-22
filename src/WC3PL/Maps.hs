module WC3PL.Maps (WC3Map,
             getFeatures,
             viewMap,
             printMap,
             buildMap) where
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.IO (IOArray)

type Grid = IOArray Int [WC3Feature]
type Location = (Int, Int)
type Dimensions = (Int, Int)

data WC3Map = WC3Map !Dimensions !Grid

instance Show WC3Map where
  show (WC3Map (x,y) grid) = "WC3Map (" ++ show x ++ ", " ++ show y ++ ")"

data WC3Feature = Goldmine
                | Tree
                | Townhall
                | Barracks
                | Peasant
                | Footman
                | Tile !Int

instance Show WC3Feature where
  show Goldmine = "g"
  show Tree = "w"
  show Townhall = "h"
  show Barracks = "b"
  show Peasant = "p"
  show Footman = "f"
  show (Tile 0) = "."
  show (Tile i) = show i

getFeatures :: WC3Map -> Location -> IO [WC3Feature]
getFeatures wc3map@(WC3Map _ grid) loc = readArray grid (translate wc3map loc)

translate :: WC3Map -> Location -> Int
translate (WC3Map (maxX,maxY) _) (x,y) | 1 <= x && x <= maxX && 1 <= y && y <= maxY = (x-1)*maxX + y
translate (WC3Map (maxX,maxY) _) (x,y) | otherwise = error $ "Tried to dereference location: (" ++ (show x) ++ ", " ++ (show y) ++ ") in map with dimensions: (" ++ (show maxX) ++ ", " ++ (show maxY) ++ ")"

viewMap :: WC3Map -> IO (String)
viewMap wc3map@(WC3Map (x, y) grid) = do
  features <- mapM (getFeatures wc3map) [(i,j) | i <- [1..x], j <- [1..y]]
  let topFeatures = map (show . head) features
  return $ snd $ foldl fuse (1, "") topFeatures
  where
    fuse (i, accum) s | i  == x = (1, accum ++ s ++ "\n")
    fuse (i, accum) s = (i + 1, accum ++ s)

printMap :: WC3Map -> IO ()
printMap x = viewMap x >>= putStrLn

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
