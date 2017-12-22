> {-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
> import Data.List
> import qualified Data.Set as S
> import Control.Arrow ((&&&),(***),first)

> newtype Grid = Grid { nodes_ :: S.Set (Int,Int) } deriving (Show)

> parse :: String -> Grid
> parse contents = let
>   -- grab the coordinates of '#'s in the file, relative to the top left
>   s = S.fromList
>     . concatMap (uncurry $ (\y xs -> (,) <$> xs <*> pure y))
>     . uncurry zip
>     . first reverse
>     . unzip
>     . zip [0..]
>     . map (map fst . filter (\case (_,'#') -> True; _ -> False) . zip [0..])
>     . lines
>     $ contents
>   -- find the center of the map
>   (cx,cy) = ((`div` 2) *** (`div` 2))
>           . (length . head &&& length)
>           . lines
>           $ contents
>   -- re-center the coordinates
>   s' = S.map (subtract cx *** subtract cy) s
>   in Grid s'

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

> data Network = N { bursts_ :: Int, virus_ :: Virus, grid_ :: Grid }
>   deriving (Show)

> begin :: Grid -> Network
> begin = N 0 (Virus (0,0) U)

> burst :: Network -> Network
> burst n = let
>   ( dir , update, count ) =
>     case (pos_ $ virus_ n) `S.member` (nodes_ . grid_ $ n) of
>       True  -> ( R, S.delete, id   )
>       False -> ( L, S.insert, succ )
>   n' = n { bursts_ = count . bursts_ $ n
>          , virus_  = move . turn dir . virus_ $ n
>          , grid_   = Grid . update (pos_ . virus_ $ n) . nodes_ . grid_ $ n }
>   in n'

> pr :: Network -> IO ()
> pr (N b (Virus (xv,yv) _) (Grid s)) = do
>   let -- ((xm,ym),(xM,yM)) = (S.findMin &&& S.findMax) s -- "wrong"
>       ((xm,ym),(xM,yM)) = ((-25,-23),(52,70)) -- tailored for 10001
>       draw (x,y) | (x,y) == (xv,yv) = '[' : draw' (x,y) : "]"
>                  | otherwise        = ' ' : draw' (x,y) : " "
>       draw' (x,y) | (x,y) `S.member` s = '#'
>                   | otherwise          = '.'
>       bursts = concat [" [ bursts: ", show b, " ]"]
>   putStrLn . (++ bursts) . intercalate "\n" $
>     [ concat [ draw (x,y) | x <- [xm..xM] ] | y <- [yM,yM-1..ym] ]

> main :: IO ()
> main = do
>   g <- parse <$> readFile "input.txt"
>   pr . last . take 10001 . iterate burst . begin $ g
