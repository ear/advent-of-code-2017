> {-# LANGUAGE LambdaCase #-}
> module Main where

> import Data.Char                       (isLetter)
> import Data.List                       (unfoldr)
> import Data.Maybe                      (mapMaybe, isJust, listToMaybe)
> import Data.Map.Strict                 (Map, (!))
> import qualified Data.Map.Strict as M
> import Control.Arrow                   ((&&&))
> import Control.Applicative             ((<|>))
> import System.Environment              (getArgs)

> newtype Diag = Diag (Map (Int,Int) Char)
>   deriving (Show)

> readDiag :: String -> Diag
> readDiag = Diag . M.fromList . concatMap line . zip [0..] . lines
>   where
>     line (y,cs) = mapMaybe (glyph y) $ zip [0..] cs
>     glyph _ (_,' ') = Nothing
>     glyph y (x,c  ) = Just ((x,y),c)

> entryPoint :: Diag -> (Int,Int)
> entryPoint (Diag m) = head . M.keys . M.filterWithKey entry $ m
>   where
>     entry (_,0) '|' = True
>     entry _     _   = False

> data Direction = R | U | L | D
>   deriving (Show)

> next :: Direction -> (Int,Int) -> (Int,Int)
> next R (x,y) = (x+1,y)
> next U (x,y) = (x,y-1)
> next L (x,y) = (x-1,y)
> next D (x,y) = (x,y+1)

> opposite :: Direction -> Direction
> opposite R = L
> opposite U = D
> opposite L = R
> opposite D = U

> (@@) :: Diag -> (Int,Int) -> Char
> (Diag m) @@ c = m ! c

> (@?) :: Diag -> (Int,Int) -> Maybe Char
> (Diag m) @? c = M.lookup c m

> neighbors :: Direction                 -- direction coming from
>           -> (Int,Int)                 -- current coordinates
>           -> [(Direction,(Int,Int))]   -- coordinates of possible places to go
> neighbors R (x,y) = [              (U,(x,y-1)), (L,(x-1,y)), (D,(x,y+1)) ]
> neighbors U (x,y) = [ (R,(x+1,y)),              (L,(x-1,y)), (D,(x,y+1)) ]
> neighbors L (x,y) = [ (R,(x+1,y)), (U,(x,y-1)),              (D,(x,y+1)) ]
> neighbors D (x,y) = [ (R,(x+1,y)), (U,(x,y-1)), (L,(x-1,y))              ]

> step :: Diag
>      -> Direction                      -- direction
>      -> (Int,Int)                      -- current coordinates
>      -> Maybe (Direction,(Int,Int))    -- coordinates to go to, if possible
> step d dir c =
>   ((d @? (next dir c)
>     >>= (\case ng | compatible dir ng -> pure (dir, next dir c)
>                   | otherwise -> fail "")))
>   <|> (listToMaybe $ filter (isJust . (d @?) . snd) (neighbors (opposite dir) c))

> travel :: Diag -> [(Int,Int)]
> travel d = entryPoint d : unfoldr (((snd &&& id) <$>) . uncurry (step d)) (D,entryPoint d)

> compatible :: Direction -> Char -> Bool
> compatible _ ' ' = False
> compatible R '-' = True
> compatible L '-' = True
> compatible _ '-' = False
> compatible U '|' = True
> compatible D '|' = True
> compatible _ '|' = False
> compatible _ '+' = True
> compatible _ c | isLetter c = True
> compatible _ _ = False

> main :: IO ()
> main = do
>   [fileName] <- getArgs
>   d <- readDiag <$> readFile fileName
>   let path = travel d
>   putStrLn . filter isLetter . map (d @@) $ path
>   print . length $ path
