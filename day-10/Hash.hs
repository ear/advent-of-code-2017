{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.List (foldl')
import Data.Foldable (toList)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S

main :: IO ()
main = print . hash 256 $ input

input :: [Int]
input = [76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229]

step :: (Int,Int,Seq Int) -> Int -> (Int,Int,Seq Int)
step (skip,i,xs) l
  = let (ls,rs) = S.splitAt i xs
        ys = rs >< ls
        (S.reverse -> ls',rs') = S.splitAt l ys
        zs = ls' >< rs'
        i' = (i + l + skip) `rem` (S.length xs)
        skip' = skip + 1
        (ls'',rs'') = S.splitAt (S.length xs - i) zs
    in (skip',i',rs'' >< ls'')

hash :: Int -> [Int] -> Int
hash n xs = x*y
  where (_,_,toList -> (x:y:_)) = foldl' step (0,0,S.fromList [0..n-1]) xs
