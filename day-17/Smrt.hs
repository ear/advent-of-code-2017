module Main where

import Data.List

s :: Int
s = 359

main :: IO ()
main = mapM_ print $
  filter (\(_,pos,_) -> pos == 1 ) $
  scanl' go (1,0,0) [1..50000000::Int]
    where
      go :: (Int,Int,Int) -> Int -> (Int,Int,Int)
      go ( size , pos , _ ) n = let
        insertPos = succ $ (pos + s) `mod` size
        in ( size+1 , insertPos , n )
