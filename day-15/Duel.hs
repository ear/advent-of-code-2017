module Main where

import Data.Word
import Data.Bits
import Data.Monoid

a,b,m :: Word64
m = 2147483647
a = 16807
b = 48271

next :: (Word64,Word64) -> (Word64,Word64)
next (xa,xb) = ( (xa * a) `mod` m, (xb * b) `mod` m )

mask :: Word64
mask = pred $ shift 16 2

test :: (Word64,Word64) -> Int
test (xa,xb) | (xa .&. mask) == (xb .&. mask) = 1
             | otherwise                      = 0

match :: [(Word64,Word64)] -> Int
match = getSum . foldMap (Sum . test)

t1 :: (Word64,Word64)
t1 = next (65,8921)

main :: IO ()
main = do
  [sa,sb] <- take 2 . map (read . last . words) . lines <$> readFile "input.txt"
  print . match . take 40000000 . iterate next $ (sa,sb)
