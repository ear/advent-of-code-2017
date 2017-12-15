Algebraic solution
==================

> {-# LANGUAGE ViewPatterns #-}
> module Main where
> import Data.Ord
> import Data.List
> import Data.Monoid
> import Control.Arrow

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

That seems fast enough if compiled with -O3. Takes 0.24s +-0.01 on my system.

Here's another idea, a sieve. First off the equations can be written as:

    x ≢ 0 (mod 4)    i.e.    x ≡ 1,2,3 (mod 4)
    x ≢ 1 (mod 2)    i.e.    x ≡ 0 (mod 2)
    x ≢ 2 (mod 6)    i.e.    x ≡ 0,1,3,4,5 (mod 6)
    x ≢ 0 (mod 6)    i.e.    x ≡ 1,2,3,4,5 (mod 6)

In general: x ≢ (-c) (mod n)

This version takes 4.04s +- 0.14. Way slower than brute force!

sieve :: [(Int,Int)] -> [Int] -> [Int]
sieve = foldl1' (.) . map (\(c,n) -> filter (\x -> (x `mod` period n) /= ((negate c) `mod` (period n))))

This version takes 0.36s +-0.02. Better but still slower than brute force.

sieve :: [(Int,Int)] -> [Int] -> [Int]
sieve = appEndo . foldMap
  (Endo . (\(c,n) -> filter (\x -> (x `mod` n) /= (negate c `mod` n))))
  . sortBy (comparing (negate . snd)) . map (second period)

Last try. Written in the simpler comprehension style. Takes 0.34s +- 0.01. Wah.

> sieve :: [(Int,Int)] -> [Int]
> sieve (sortBy (comparing snd) . map (second period) -> cns)
>   = [ x | x <- [0..], all (test x) cns ]

> test :: Int -> (Int,Int) -> Bool
> test x (c,n) = (x `mod` n) /= (negate c `mod` n)

Changing all to any and test's /= to == does not change the results.
Out of ideas.

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
>   -- print . sum . map severity $ xs -- part 1
>   -- print . search $ xs -- part 2, bruteforce
>   -- print . head . sieve xs $ [0..] -- part 2, sieve
>   print . head . sieve $ xs -- part 2, sieve

> input :: IO [(Int,Int)]
> input = map (parse . words) . lines <$> readFile "input.txt"

> parse :: [String] -> (Int,Int)
> parse ((reverse -> (':':(reverse -> layer))):[depth]) = (read layer, read depth)
> parse _ = error "parse error"
