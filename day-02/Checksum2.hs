module Main where

import Data.List ((\\))
import Control.Monad (guard, forM)

main :: IO ()
main = do
  input <- readFile "input2.txt"
  let ls = map (map read . words) . lines $ input :: [[Int]]
  ps <- forM ls $ \l -> do
          forM l $ \n -> do
            let l' = l \\ [n]
            print l
            print (n,l')
            return $ do x <- l'
                        guard (n `rem` x == 0)
                        return (n,x)
  let ps' = concat . concat . map (filter (not . null)) $ ps
  let checksum2 = sum . map (uncurry (div)) $ ps'
  print ps'
  print checksum2
  -- let checksum = sum . map (uncurry (-) . head . findDivisiblePairs) $ ls
  -- print checksum

findDivisiblePairs :: [Int] -> [(Int,Int)]
findDivisiblePairs xs = do
  x <- xs
  y <- xs \\ [x]
  guard (x `rem` y == 0)
  return (x,y)



-- search :: [Int] -> Maybe ((Int,Int), [Int])
-- search [] = Nothing
-- search (x:xs) = case find ((0==) . (`rem` x)) xs of
--   Nothing -> search xs
--   Just y  -> Just ((x,y), xs)

