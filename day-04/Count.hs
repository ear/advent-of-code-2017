module Main where

import Data.List (sort, nub)

main = readFile "input.txt" >>= print . count . map words . lines

count :: [[String]] -> Int
count = sum . map valid

valid :: [String] -> Int
valid xs | sort xs == nub (sort xs) = 1
         | otherwise = 0
