{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H

main :: IO ()
main = print . go . fromList $ xs

xs :: [Int]
len :: Int

xs = [5,1,10,0,1,7,13,14,3,12,8,10,7,12,0,6]
-- xs = [0,2,7,0]
len = length xs

memory :: Mem
memory = fromList xs

newtype Mem = Mem [Int]
  deriving (Hashable, Eq)

fromList :: [Int] -> Mem
fromList = Mem

toList :: Mem -> [Int]
toList (Mem xs) = xs

findMax :: (Ord a) => [a] -> (Int, a)
findMax [] = error "empty list"
findMax (x:xs) = (\(_,i,v) -> (i,v)) $ foldl f (0,0,x) xs
  where
    -- (current index, maximum's index, maximum's value)
    f (n,m,x) y = (n',m',x')
      where
        n' = n+1
        m' | y > x = n' | otherwise = m
        x' | y > x = y  | otherwise = x

mask :: Mem -> [Int]
mask mem = wrap subtraction additions
  where
    (n,m) = findMax (toList mem)
    subtraction = replicate n 0 ++ [-m] ++ replicate (len - n - 1) 0
    additions = replicate (n+1) 0 ++ replicate m 1
    wrap xs [] = xs
    wrap xs ys = wrap (zipWith (+) xs (extend ls)) rs
      where
        (ls,rs) = splitAt len ys

extend :: [Int] -> [Int]
extend xs = take len $ xs ++ repeat 0

redistribute :: Mem -> Mem
redistribute mem = mem'
  where
    mem' = fromList $ zipWith (+) (toList mem) (mask mem)

-- Check for duplicates

go = walk (H.empty) . zip [0..] . iterate redistribute
  where
    walk :: HashMap Mem Int -> [(Int, Mem)] -> Int
    walk seen ((i,m):ms) | H.member m seen = let Just i' = H.lookup m seen
                                             in i - i'
                         | otherwise       = walk (H.insert m i seen) ms

