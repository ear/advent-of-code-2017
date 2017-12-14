{-# LANGUAGE ViewPatterns #-}
module KnotHash (knothash) where

import Data.Bits
import Data.Char
import Data.List (foldl', foldl1', unfoldr)
import Data.Foldable (toList)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import Numeric (showHex)

knothash :: String -> String
knothash = formatHash . denseHash . sparseHash 256 . parse

parse :: String -> [Int]
parse = (++ [17, 31, 73, 47, 23]) . map ord

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

hashRound :: (Int, Int, Seq Int) -> [Int] -> (Int, Int, Seq Int)
hashRound = foldl' step

sparseHash :: Int -> [Int] -> [Int]
sparseHash n = end . foldl' hashRound (0,0,begin) . replicate 64
  where
    begin = S.fromList [0 .. n-1]
    end (_,_,x) = toList x

denseHash :: [Int] -> [Int]
denseHash = unfoldr go
  where
    go [] = Nothing
    go xs = let (ls,rs) = splitAt 16 xs
            in Just (foldl1' xor ls, rs)

show02x :: Int -> String
show02x (flip showHex "" -> xs) = reverse . take 2 . reverse . ('0':) $ xs

formatHash :: [Int] -> String
formatHash = (>>= show02x)
