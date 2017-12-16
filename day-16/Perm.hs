{-# LANGUAGE ExistentialQuantification #-}
module Perm where

import Data.Word
import Data.List
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

import Control.Arrow

type Perm = U.Vector Int

p :: Vector Int
p = U.fromList [1,0,3,4,2]

one :: Int -> Perm
one n = U.fromList [0 .. pred n]

identity :: Perm -> Perm
identity = one . U.length

period :: Perm -> Int
period p = search 0 one
  where
    one = identity p
    search :: Int -> Perm -> Int
    search n q | n > 0 && q == one = n
               | otherwise         = search (n+1) (U.backpermute q p)

power :: Perm -> Int -> Perm
power p n = go n one
  where
    one = identity p
    go 0 q = q
    go n q = go (n-1) (U.backpermute q p)

permFromList :: [Int] -> Perm
permFromList = U.fromList

permAsList :: Perm -> [Int]
permAsList = U.toList

apply :: Perm -> Perm -> Perm
apply = U.backpermute

listFromPerm :: Perm -> [(Int,Int)]
listFromPerm = zip [(0::Int)..] . U.toList

-- assuming the list is complete
-- i.e. each pair appears with its own symmetric twin
permFromPairs :: [(Int,Int)] -> Perm
permFromPairs = foldl' modify (U.fromList [0..15])
  where
    modify v (i,x) = U.modify (\w -> M.write w i x) v
