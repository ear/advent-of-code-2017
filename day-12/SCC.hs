{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Graph
import Control.Arrow

main :: IO ()
main = do
  pipes <- map (parse . words) . lines <$> readFile "input.txt"
  putStr "(min_id,max_id) = "
  let bounds = (minimum &&& maximum) . fst . unzip $ pipes
  print bounds
  let es = pipes >>= uncurry (map . (,)) -- edges
  let g = buildG bounds es
  print . length . reachable g $ 0
  print . length . scc $ g

parse :: [String] -> (Int,[Int])
parse (v:"<->":(map dropComma -> vs)) = (read v, map read vs)
parse _ = error "unexpected input"

dropComma :: String -> String
dropComma [] = error "can't drop comma"
dropComma (reverse -> ',':xs) = reverse xs
dropComma xs = xs
