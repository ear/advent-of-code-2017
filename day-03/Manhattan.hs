module Main where

import Data.List
import Data.Maybe

squares, bottomRights, sides :: [Int]

-- sizes of concentric squares around the origin
-- [1,8,16,24,32,40,48,56,64,72,...]
squares = 1 : 8 : map ((4*) . (2+) . (`div` 4)) (tail squares)

-- bottom-right corner of each square
-- [1,9,25,49,81,121,169,225,289,361,...]
bottomRights = scanl1 (+) squares

-- length of the side of each square
-- [1,3,5,7,9,11,13,15,17,19,...]
sides = 1 : 3 : map (2+) (tail sides)

-- x/y travel from center to bottomRights of each square
-- [(0,0),(2,2),(4,4),(6,6),(8,8),(10,10),(12,12),(14,14),(16,16),(18,18),...]
bottomRightTravel :: [(Int,Int)]
bottomRightTravel = map (\x -> (x,x)) [0..]

-- which square does a number live on (i.e. the square's index)
findSquare :: Int -> Int
findSquare n = fromJust . findIndex (n<=) $ bottomRights

-- x/y travel from a bottom-right to each of the numbers in its square
-- e.g. xysForSquare 1
-- ==> [(0,0),(-1,0),(-2,0),(-2,-1),(-2,-2),(-1,-2),(0,-2),(0,-1)]
xysForSquare :: Int -> [(Int,Int)]
xysForSquare index = scanl (\(x,y) (a,b) -> (x+a,y+b)) (0,0) (zip xs ys)
  where
    xs = replicate n (-1) ++ replicate n 0 ++ replicate n 1 ++ replicate (n-1) 0
    ys = replicate n 0 ++ replicate n (-1) ++ replicate n 0 ++ replicate (n-1) 1
    n = pred $ sides !! index

-- x/y travel from a bottom-right to a specific number in its own square
-- e.g. xyForNumber 5 ==> (-2,-2) -- position relative to 9, its bottom-right
xyForNumber :: Int -> (Int,Int)
xyForNumber n = xysForSquare squareIndex !! (bottomRights !! squareIndex - n)
  where
    squareIndex = findSquare n

-- x/y travel from the center to a specific number
-- e.g. travel 5 ==> (-1,-1)
travel :: Int -> (Int,Int)
travel n = (x1+x2,y1+y2)
  where
    (x1,y1) = xyForNumber n                     -- travel from b-r to n
    (x2,y2) = bottomRightTravel !! findSquare n -- travel from center to b-r

-- manhattan distance of a number from the center
manhattan :: Int -> Int
manhattan n = abs x + abs y
  where
    (x,y) = travel n

main :: IO ()
main = readFile "input.txt" >>= print . manhattan . read
