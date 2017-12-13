{-# LANGUAGE ViewPatterns #-}
module Main where

main :: IO ()
main = do
  input <- r
  print . severity $ input

r :: IO [(Int,Int)]
r = map (parse . words) . lines <$> readFile "input.txt"

parse :: [String] -> (Int,Int)
parse ((reverse -> (':':(reverse -> layer))):[depth]) = (read layer, read depth)
parse _ = error "parse error"

ys :: [(Int,Int)]
ys = [(0,3),(1,2),(4,4),(6,4)]

f :: (Int,Int) -> Int
f (layer,depth) = sweep depth !! layer

sweep depth = cycle $ [0..depth-1] ++ reverse (drop 1 $ init [0..depth-1])

severity :: [(Int,Int)] -> Int
severity xs = sum . map (uncurry (*) . fst) . filter ((0==) . snd) . tail . zip xs $ map f xs

-- 4: 1 2 3 2 1 0
--  0 1 2 3 2 1 0 1 2 3 2 1 0 1 2 3 2 1 0 ...

-- parity layer depth
--   = let (n,r) = divMod layer range in
--     case (even n) of
--       True -> r
--       False -> depth-r
--   where
--     range | even depth = depth
--           | otherwise  = depth-1
