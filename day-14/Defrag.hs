{-# LANGUAGE ViewPatterns #-}
module Main where

import KnotHash

import Numeric
import Data.List (foldl')
import Data.Maybe
import Data.Graph
import Data.Monoid
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  let grid = fromSeed "wenycdww"
  print $ countUsed grid
  print $ countRegions grid

type Coord = (Int,Int)

data Grid = Grid { grid_ :: Map Coord Int, width_ :: Int, height_ :: Int }

fromSeed :: String -> Grid
fromSeed seed =
  toGrid $ map (toBits . knothash . (seed ++) . ('-':) . show) [(0 :: Int)..127]
  where
    toGrid bs = Grid ((\(_,_,m) -> m) $ foldl' go (0,0,M.empty) (concat bs)) h w
      where
        go (x,y,m) v
          | x == (w-1) = (0,y+1,M.insert (x,y) v m)
          | otherwise  = (x+1,y,M.insert (x,y) v m)
        h = length bs
        w = length (head bs)

toBits :: String -> [Int]
toBits = (>>= ((table !!) . fst . head . readHex . pure))

table :: [[Int]]
table = [[a,b,c,d] | a<-[0,1], b<-[0,1], c<-[0,1], d<-[0,1]]

countUsed :: Grid -> Int
countUsed = getSum . foldMap Sum . grid_

at :: Grid -> Int -> Int -> Int
at (grid_ -> bits) x y = bits ! (x,y)

inside :: Grid -> Coord -> Bool
inside (Grid _ w h) (x,y) = 0 <= x && x < w && 0 <= y && y < h

live :: Grid -> Coord -> Bool
live grid (x,y) = 1 == at grid x y

adjacents :: Grid -> [(Coord,Coord,[Coord])]
adjacents grid = [ ((x,y), (x,y), adjacent grid x y)
                 | y <- [0 .. (height_ grid-1)]
                 , x <- [0 .. (width_ grid-1)]
                 , live grid (x,y) ]

adjacent :: Grid -> Int -> Int -> [(Int,Int)]
adjacent grid x y = mapMaybe collect neighbors
  where
    neighbors = filter (inside grid) [ (x+1,y), (x,y-1), (x-1,y), (x,y+1) ]
    collect c | live grid c = Just c
              | otherwise   = Nothing

countRegions :: Grid -> Int
countRegions = length . scc . (\(g,_,_) -> g) . graphFromEdges . adjacents
