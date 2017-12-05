{-# LANGUAGE TypeApplications #-}
module Main where

import Data.List

main :: IO ()
main = do
  maze <- enter . map (read @Int) . lines <$> readFile "input.txt"
  print $ go problem1 maze
  print $ go problem2 maze

problem1 = succ
problem2 x | x > 2     = x-1
           | otherwise = x+1

data Maze a = In { left_ :: [a], at_ :: a, right_ :: [a] } | Out

instance Show a => Show (Maze a) where
  show Out = "Out"
  show (In ls a rs) = concat $ intersperse " " $ tokens
    where tokens = (map show . reverse) ls ++ ["(" ++ show a ++ ")"] ++ map show rs

enter :: [a] -> Maze a
enter []     = Out
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

-- Travel a maze using the given update function
-- Returns the number of steps to get out

go :: (Int -> Int) -> Maze Int -> Int
go update = fst . last . go' 0 update

go' :: Int -> (Int -> Int) -> Maze Int -> [(Int,Maze Int)]
go' n _      Out = [(n,Out)]
go' n update m@(In ls x rs) = (n,m) : go' (n+1) update m''
  where
    m' = (In ls (update x) rs)
    move | x >  0 = r x
         | x == 0 = id
         | x <  0 = l (-x)
    m'' = move m'
