module Main where

import Control.Arrow

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ls = map (map read . words) . lines $ input :: [[Int]]
  let checksum = sum . map (uncurry (-) . (maximum &&& minimum)) $ ls
  print checksum
