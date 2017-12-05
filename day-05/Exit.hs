{-# LANGUAGE TypeApplications #-}
module Main where

import Data.List

main = readFile "input.txt" >>= print . go . enter . map (read @Int) . lines

data Maze a = In { left_ :: [a], at_ :: a, right_ :: [a] } | Out
  deriving (Show)

enter :: [a] -> Maze a
enter (x:xs) = In [] x xs

-- Single movements

right :: Maze a -> Maze a
right (In _  _ []    ) = Out
right (In ls x (r:rs)) = In (x:ls) r rs
right Out = Out

left :: Maze a -> Maze a
left (In []     _ _ ) = Out
left (In (l:ls) x rs) = In ls l (x:rs)
left Out = Out

-- Move N times in one direction

r :: Int -> Maze a -> Maze a
r 0 m = m
r n m = r (n-1) (right m)

l :: Int -> Maze a -> Maze a
l 0 m = m
l n m = l (n-1) (left m)

-- Travel a maze, return the number of steps to get out

go :: Maze Int -> Int
go = fst . last . go' 0

go' :: Int -> Maze Int -> [(Int,Maze Int)]
go' n Out = [(n,Out)]
go' n m@(In ls x rs) = (n,m) : go' (n+1) m''
  where
    m' = (In ls (x+1) rs)
    move | x >  0 = r x
         | x == 0 = id
         | x <  0 = l (-x)
    m'' = move m'
