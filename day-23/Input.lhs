> {-# LANGUAGE TypeApplications #-}
> import Data.List
> import Data.Array

divisors :: Integer -> [Integer]
divisors n
  | n < 0 = divisors (negate n)
  | otherwise = ds
    where
      ds = nub $ sort $ concat [[x, div n x] | x <- [2 .. limit], rem n x == 0]
      limit = floor $ sqrt $ fromInteger n

> primesFromToA :: Int -> Int -> [Int]
> primesFromToA a b = (if a<3 then [2] else [])
>                       ++ [i | i <- [o,o+2..b], ar ! i]
>   where
>     o  = max (if even a then a+1 else a) 3   -- first odd in the segment
>     r  = floor . sqrt @Double $ fromIntegral b + 1
>     ar = accumArray (\_ _ -> False) True (o,b)  -- initially all True,
>           [(i,()) | p <- [3,5..r]
>                     , let q  = p*p      -- flip every multiple of an odd
>                           s  = 2*p                         -- to False
>                           (n,x) = quotRem (o - q) s
>                           q2 = if  o <= q  then q
>                                else  q + (n + signum x)*s
>                     , i <- [q2,q2+s..b] ]

> main :: IO ()
> main = print . length $
>   [108100,108100+17..108100+17000] \\ (primesFromToA 108100 (108100+17000))
