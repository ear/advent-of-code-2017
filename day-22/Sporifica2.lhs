> {-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
> import Data.List
> import qualified Data.Map.Strict as M
> import Control.Arrow ((&&&),(***),first)

> data Cell = Weakened | Infected | Flagged deriving (Show, Eq)

> newtype Grid = Grid { nodes_ :: M.Map (Int,Int) Cell } deriving (Show)

> parse :: String -> Grid
> parse contents = let
>   -- find the center of the map
>   (cx,cy) = ((`div` 2) *** (`div` 2))
>           . (length . head &&& length)
>           . lines
>           $ contents
>   -- map of the '#'s in the file with the center-adjusted coordinates
>   m = M.fromList
>     . map ( \(x,y) -> ((x-cx,y-cy),Infected) )
>     . concatMap (uncurry $ (\y xs -> (,) <$> xs <*> pure y))
>     . uncurry zip
>     . first reverse
>     . unzip
>     . zip [0..]
>     . map (map fst . filter (\case (_,'#') -> True; _ -> False) . zip [0..])
>     . lines
>     $ contents
>   in Grid m

> data Direction = R | U | L | D deriving (Show, Eq, Enum)

> left, right :: Direction -> Direction
> left  = toEnum . (`mod` 4) . succ . fromEnum
> right = toEnum . (`mod` 4) . pred . fromEnum

> turn :: Direction -> Virus -> Virus
> turn L (Virus coord dir) = Virus coord (left  dir)
> turn R (Virus coord dir) = Virus coord (right dir)
> turn _ _ = error "err… turn… what?"

> move :: Virus -> Virus
> move (Virus (x,y) R) = Virus (x+1,y) R
> move (Virus (x,y) U) = Virus (x,y+1) U
> move (Virus (x,y) L) = Virus (x-1,y) L
> move (Virus (x,y) D) = Virus (x,y-1) D

> data Virus = Virus { pos_ :: (Int,Int), dir_ :: Direction } deriving (Show)

> around :: Virus -> Virus
> around (Virus c d) = Virus c (toEnum . (`mod` 4) . succ . succ . fromEnum $ d)

> data Network = N { bursts_ :: Int, virus_ :: Virus, grid_ :: Grid }
>   deriving (Show)

> begin :: Grid -> Network
> begin = N 0 (Virus (0,0) U)

> burst :: Network -> Network
> burst n = let
>   ( turn', update, count ) = let coords = pos_ $ virus_ n in
>     case (pos_ $ virus_ n) `M.lookup` (nodes_ . grid_ $ n) of
>       Nothing       -> ( turn L, M.insert coords Weakened, id   )
>       Just Weakened -> ( id    , M.insert coords Infected, succ )
>       Just Infected -> ( turn R, M.insert coords Flagged , id   )
>       Just Flagged  -> ( around, M.delete coords         , id   )
>   n' = n { bursts_ = count . bursts_ $ n
>          , virus_  = move . turn' . virus_ $ n
>          , grid_   = Grid . update . nodes_ . grid_ $ n }
>   in n'

The bounds are "wrong" because they are just from `Sporifica.lhs`.

> pr :: Network -> IO ()
> pr (N b (Virus (xv,yv) _) (Grid m)) = do
>   let -- ((xm,ym),(xM,yM)) = (S.findMin &&& S.findMax) s -- "wrong"
>       ((xm,ym),(xM,yM)) = ((-25,-23),(52,70)) -- tailored for 10001
>       draw (x,y) | (x,y) == (xv,yv) = '[' : draw' (x,y) : "]"
>                  | otherwise        = ' ' : draw' (x,y) : " "
>       draw' c = case c `M.lookup` m of
>                   Nothing       -> '.'
>                   Just Weakened -> 'W'
>                   Just Infected -> '#'
>                   Just Flagged  -> 'F'
>       bursts = concat [" [ bursts: ", show b, " ]"]
>   putStrLn . (++ bursts) . intercalate "\n" $
>     [ concat [ draw (x,y) | x <- [xm..xM] ] | y <- [yM,yM-1..ym] ]

> main :: IO ()
> main = do
>   g <- parse <$> readFile "input.txt"
>   pr . last . take 10000001 . iterate burst . begin $ g

TODO: there seems to be some big memory leak? Or not? Investigate!
