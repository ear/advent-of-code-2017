module Main where

import KnotHash

import Numeric
import Data.Monoid

import Data.Maybe
import Data.Graph

main :: IO ()
main = do
  let g = grid "wenycdww"
  print . countUsed $ g
  print . countConnectedComponents $ g

type Grid = [[Int]]

grid :: String -> Grid
grid seed = map (toBits . knothash . (seed ++) . ('-':)) (map show [(0 :: Int)..127])

toBits :: String -> [Int]
toBits = (>>= ((table !!) . fst . head . readHex . pure))

table :: [[Int]]
table = [[a,b,c,d] | a<-[0,1], b<-[0,1], c<-[0,1], d<-[0,1]]

countUsed :: Grid -> Int
countUsed = getSum . foldMap (Sum . getSum . foldMap Sum)

at :: Grid -> Int -> Int -> Int
at bits x y = (bits !! y) !! x

type Coord = (Int,Int)

adjacents :: Grid -> [(Coord,Coord,[Coord])]
adjacents bits = let height = length bits; width = length (head bits) in
   [ ((x,y),(x,y),adjacent bits height width x y)
         | y <- [0..height-1], x <- [0..width-1], 1 == at bits x y ]

adjacent :: Grid -> Int -> Int -> Int -> Int -> [(Int,Int)]
adjacent bits height width x y = mapMaybe live neighbors
  where
    neighbors = filter inside [ (x+1,y), (x,y-1), (x-1,y), (x,y+1) ]
    inside (x_,y_) = 0 <= x_ && x_ < width && 0 <= y_ && y_ < height
    live (x1,y1) | 1 == at bits x1 y1 = Just (x1,y1)
                 | otherwise          = Nothing

countConnectedComponents :: Grid -> Int
countConnectedComponents = length . scc . (\(g,_,_) -> g) . graphFromEdges . adjacents
