module Main where

import KnotHash

import Numeric
import Data.Monoid

main :: IO ()
main = print . countUsed . grid $ "wenycdww"

grid :: String -> [[Int]]
grid seed = map (toBits . knothash . (seed ++) . ('-':)) (map show [(0 :: Int)..127])

toBits :: String -> [Int]
toBits = (>>= ((table !!) . fst . head . readHex . pure))

table :: [[Int]]
table = [[a,b,c,d] | a<-[0,1], b<-[0,1], c<-[0,1], d<-[0,1]]

countUsed :: [[Int]] -> Int
countUsed = getSum . foldMap (Sum . getSum . foldMap Sum)
