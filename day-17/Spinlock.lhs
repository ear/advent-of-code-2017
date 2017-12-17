> module Main where

Not sure I am going to need these:

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U

This one has log(n) findMax! But mapKeys is n*log(n) hehe!

> import Data.Map.Strict (Map, (!))
> import qualified Data.Map.Strict as M

> import Data.List (foldl')

> data Mem = M { mem_ :: !(Map Int Int), pos_ :: !Int } deriving Show

> empty :: Mem
> empty = M { mem_ = M.singleton 0 0, pos_ = 0 }

> size_ :: Mem -> Int
> size_ = M.size . mem_

> step :: Int
> step = 359

To move forward just rotate pos_ cyclically through the map.

> forward :: Mem -> Mem
> forward m = m { pos_ = ((pos_ m) + step) `mod` (size_ m) }

To insert:

  - calculate the insert position
  - if it's at the end just insert and move there
  - if not, slide all the >= keys one forward and plop the number in there.

> insert :: Mem -> Int -> Mem
> insert m@(M mem pos) x = let
>   insertPos = succ pos -- two cases, either at the end, or inside the memory
>   in case M.lookupGE insertPos mem of
>     Nothing -> m { mem_ = M.insert insertPos x mem, pos_ = insertPos }
>     Just (i,_) -> m {
>       mem_ = M.insert insertPos x (slide_ i mem),
>       pos_ = insertPos
>     }

> slide_ :: Int -> Map Int Int -> Map Int Int
> slide_ i m = M.mapKeys go m
>   where
>     go k | k >= i = succ k | otherwise = k

> main :: IO ()
> main = let
>   M m p = foldl' (insert . forward) empty [1..2017]
>   in print $ m ! (succ p)
