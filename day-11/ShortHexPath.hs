module Main where
import Data.List (unfoldr, foldl')

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr "steps in path: "
  let path = parse input
  print $ length path
  putStr "end point: "
  let end = travel path
  print end
  putStr "distance (steps) from the origin: "
  print $ dist origin end

-- Cube coordinates for an hexagonal grid

data Coord = C { x_ :: Int, y_ :: Int, z_ :: Int }
  deriving (Show)

coord :: String -> Coord
coord "n"  = C   0   1 (-1)
coord "nw" = C   1   0 (-1)
coord "sw" = C   1 (-1)  0
coord "s"  = C   0 (-1)  1
coord "se" = C (-1)  0   1
coord "ne" = C (-1)  1   0
coord _    = error "unexpected direction string"

-- Parse directions
-- e.g. "ne,s,sw,..." -> [Coord]

parse :: String -> [Coord]
parse = unfoldr go
  where
    go (',':xs) = go xs
    go [] = Nothing
    go xs = Just (coord name, rest)
      where
        (name,rest) = break (','==) xs

-- Travel starting from the origin

origin :: Coord
origin = C 0 0 0

travel :: [Coord] -> Coord
travel = foldl' go origin
  where
    go (C x y z) (C dx dy dz) = C (x+dx) (y+dy) (z+dz)

-- The distance is the coordinate whose absolute value is the sum of the others'

dist :: Coord -> Coord -> Int
dist (C ax ay az) (C bx by bz) = maximum . map abs $ [ax-bx,ay-by,az-bz]
