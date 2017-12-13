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

> tripSeverity :: [(Int,Int)] -> Int
> tripSeverity = sum . map severity

So we have congruences

0 ≡ 0 (mod 4)
1 ≡ 0 (mod 2)
4 ≡ 0 (mod 6)
6 ≡ 0 (mod 6)

We want to find the smallest x such that, at the same time:

x + 0 ≢ 0 (mod 4)
x + 1 ≢ 0 (mod 2)
x + 4 ≢ 0 (mod 6)
x + 6 ≢ 0 (mod 6)

Let's try brute force first. The general form of the equation is:

x + c ≢ 0 (mod n)

> search :: [(Int,Int)] -- [(c,n)]
>        -> Int         -- x
> search (map (\(layer,depth) -> (layer, period depth)) -> pairs)
>   = head . dropWhile (satisfiesAny pairs) $ [0..]

> satisfiesAny :: [(Int,Int)] -> Int -> Bool
> satisfiesAny pairs x = any (\(c,n) -> (x+c)`mod`n == 0) pairs

Tests:

> ys :: [(Int,Int)]
> ys = [(0,3),(1,2),(4,4),(6,4)]

> t1, t2 :: Bool
> t1 = 24 == tripSeverity ys
> t2 = 10 == search ys

Main program: read input.txt, parse into pairs of numbers, print the solutions.

> main :: IO ()
> main = do
>   xs <- input
>   print . sum . map severity $ xs
>   print . search $ xs

> input :: IO [(Int,Int)]
> input = map (parse . words) . lines <$> readFile "input.txt"

> parse :: [String] -> (Int,Int)
> parse ((reverse -> (':':(reverse -> layer))):[depth]) = (read layer, read depth)
> parse _ = error "parse error"
