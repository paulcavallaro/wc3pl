module WC3PL.Maps (WC3Map,
             getFeatures,
             buildMap) where
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO (IOArray)

type Grid = IOArray Int [WC3Feature]
type Location = (Int, Int)
type Dimensions = (Int, Int)

data WC3Map = WC3Map Dimensions Grid

data WC3Feature = Goldmine
                | Tree
                | Townhall
                | Barracks
                | Peasant
                | Footman
                | Tile Int

getFeatures :: WC3Map -> Location -> IO [WC3Feature]
getFeatures map@(WC3Map _ grid) loc = readArray grid (translate map loc)

translate :: WC3Map -> Location -> Int
translate (WC3Map (maxX,maxY) _) (x,y) | 1 <= x && x <= maxX && 1 <= y && y <= maxY = (maxX-1)*x + y
translate (WC3Map (maxX,maxY) _) (x,y) | otherwise = error $ "Tried to dereference location: (" ++ (show x) ++ ", " ++ (show y) ++ ") in map with dimensions: (" ++ (show maxX) ++ ", " ++ (show maxY) ++ ")"

buildMap :: String -> IO WC3Map
buildMap = undefined