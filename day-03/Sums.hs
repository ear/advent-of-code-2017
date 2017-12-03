{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Foldable

import qualified Data.Map as M

-- Helpers

type Grid = M.Map (Int,Int) Int

begin :: Grid
begin = M.fromList [((0,0), 1)]

sumPair (a,b) (c,d) = (a+c,b+d)

-- Travel around the spiral

directions = cycle [(1,0),(0,1),(-1,0),(0,-1)]

times = [1..] >>= replicate 2 -- [1,1,2,2,3,3,4,4,...]

moves = concat $ zipWith replicate times directions

-- [(0,0),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1),(2,-1),(2,0),…]
coordinates = tail $ scanl sumPair (0,0) moves

-- Populate spiral

spiral = scanl populate begin coordinates

populate :: Grid -> (Int,Int) -> Grid
populate g (x,y) = M.insert (x,y) (sumNeighbours g (x,y)) g

sumNeighbours :: Grid -> (Int,Int) -> Int
sumNeighbours g (x,y) = getSum neighbours
  where
    neighbours = fold . foldMap ((Sum <$>) . (`M.lookup` g)) $ oneAway (x,y)

oneAway :: (Int,Int) -> [(Int,Int)]
oneAway (x,y) = [ (a,b) | a <- [x-1..x+1], b <- [y-1..y+1] ] \\ [ (x,y) ]

-- Find the first map that has a value larger than a given one

firstGT :: [Grid] -> Int -> Int
firstGT g n = fromJust . find (> n) . M.elems . head . dropWhile allLE $ g
  where
    allLE = null . M.filter (> n)

-- Main

main = readFile "input2.txt" >>= print . firstGT spiral . read
