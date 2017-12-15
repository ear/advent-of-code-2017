{-# LANGUAGE LambdaCase #-}
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
mask = pred $ shift 1 16

test :: (Word64,Word64) -> Int
test (xa,xb) | (xa .&. mask) == (xb .&. mask) = 1
             | otherwise                      = 0

match :: [(Word64,Word64)] -> Int
match = getSum . foldMap (Sum . test)

t1 :: (Word64,Word64)
t1 = next (65,8921)

main :: IO ()
main = do
  -- print . match . take 40000000 . iterate next $ t1
  (sa,sb) <- (\[x,y] -> next (x,y)) . take 2 . map (read . last . words) . lines
    <$> readFile "input.txt"
  print . match . take 40000000 . iterate next $ (sa,sb)
  -- print . match . take 5000000 $ zip (produce a 4 65) (produce b 8 8921)
  print . match . take 5000000 $ zip (produce a 4 sa) (produce b 8 sb)

produce :: Word64 -> Word64 -> Word64 -> [Word64]
produce factor n = filter (\x -> mod x n == 0) . iterate (\x -> mod (x*factor) m)

-- bits :: Word64 -> [Int]
-- bits = map (\case True -> 1; False -> 0) . (`map` [31,30..0]) . testBit
