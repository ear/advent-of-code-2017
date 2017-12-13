Algebraic solution
==================

> {-# LANGUAGE ViewPatterns #-}
> module Main where

The "security scanners" sweep their layers back and forth.
This is equivalent to going in circle around a deeper layer.

 depth | sweep           | cycle            | period
-------+-----------------+------------------+-----------------
 1     | 0               | 0                | 1 = (1 * 2) - 2
 2     | 0 1             | 0 1              | 2 = (2 * 2) - 2
 3     | 0 1 2 1         | 0 1 2 3          | 4 = (3 * 2) - 2
 4     | 0 1 2 3 2 1     | 0 1 2 3 4 5      | 6 = (4 * 2) - 2
 5     | 0 1 2 3 4 3 2 1 | 0 1 2 3 4 5 6 7  | 8 = (5 * 2) - 2

> period :: Int -> Int
> period = pred . pred . (2*)

Being caught in a layer means: layer ≡ 0 (mod period)
The severity for being caught in a layer is the product layer * depth.

> severity :: (Int,Int) -> Int
> severity (layer,depth)
>   | layer `mod` (period depth) == 0 = layer * depth
>   | otherwise                       = 0

Test data, parsing, and main

> ys :: [(Int,Int)]
> ys = [(0,3),(1,2),(4,4),(6,4)]

> main :: IO ()
> main = input >>= print . sum . map severity

> input :: IO [(Int,Int)]
> input = map (parse . words) . lines <$> readFile "input.txt"

> parse :: [String] -> (Int,Int)
> parse ((reverse -> (':':(reverse -> layer))):[depth]) = (read layer, read depth)
> parse _ = error "parse error"
