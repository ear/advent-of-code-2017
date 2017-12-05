{-# LANGUAGE TypeApplications #-}
module Main where

import Data.List
import Data.Maybe

main :: IO ()
main = do
  maze <- enter . map (read @Int) . lines <$> readFile "input.txt"
  print $ go problem1 maze
  print $ go problem2 maze

problem1, problem2 :: Int -> Int
problem1 = succ
problem2 x | x > 2     = x-1
           | otherwise = x+1

data Maze a = In { left_ :: [a], at_ :: a, right_ :: [a] } | Out
  deriving Eq

instance Show a => Show (Maze a) where
  show Out = "Out"
  show (In ls a rs) = concat $ intersperse " " $ tokens
    where tokens = (map show . reverse) ls ++ ["(" ++ show a ++ ")"] ++ map show rs

enter :: [a] -> Maze a
enter []     = Out
enter (x:xs) = In [] x xs

-- Single movements

right1 :: Maze a -> Maze a
right1 (In _  _ []    ) = Out
right1 (In ls x (r:rs)) = In (x:ls) r rs
right1 Out = Out

left1 :: Maze a -> Maze a
left1 (In []     _ _ ) = Out
left1 (In (l:ls) x rs) = In ls l (x:rs)
left1 Out = Out

-- Move N times in one direction

right :: Int -> Maze a -> Maze a
right 0 m = m
right n m = right (n-1) (right1 m)

left :: Int -> Maze a -> Maze a
left 0 m = m
left n m = left (n-1) (left1 m)

-- Travel a maze using the given update function
-- Returns the number of steps to get out

go :: (Int -> Int) -> Maze Int -> Int
go update = fromJust . findIndex (Out ==) . go' update

go' :: (Int -> Int) -> Maze Int -> [Maze Int]
go' _         Out         = [Out]
go' update m@(In ls x rs) = m : go' update m'
  where
    move | x >  0 = right x
         | x == 0 = id
         | x <  0 = left (-x)
    m' = move $ In ls (update x) rs
