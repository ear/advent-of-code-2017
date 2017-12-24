> import qualified Data.Set as S
> import Data.Ord
> import Data.List
> import Data.Monoid
> import Data.Function
> import Control.Monad
> import Control.Arrow

> newtype Component = C (Int,Int) deriving (Ord, Eq)

> instance Show Component where show (C (a,b)) = show a ++ '/' : show b

> unC :: Component -> (Int,Int)
> unC (C c) = c

> hasPort :: Int -> Component -> Bool
> hasPort n = uncurry (||) . ((n==) *** (n==)) . unC

orients the second argument to agree with the first's end-"port".

> orient :: Component -> Component -> Component
> orient (C (_,b)) c@(C (x,y))
>   | b == x    = c
>   | otherwise = C (y,x)

this is pretty… alas not useful:
connectsTo :: Component -> Component -> Bool
connectsTo (C (a,b)) = (||) <$> hasPort a <*> hasPort b

> parse :: String -> [Component]
> parse = map C
>       . sort . map (sortPair . second (read . tail) . head . reads) . lines
>   where sortPair (x,y) | x > y = (y,x) | otherwise = (x,y)

Operating under the assumption that there are no duplicates. It is true for both
the test and the given input. Gonna need a MultiSet otherwise.

> type Queue = S.Set Component

> enqueue :: [Component] -> Queue
> enqueue = S.fromList

> type Bridge = [Component]

> bridges :: [Component] -> [Bridge]
> bridges cs = let
>   q = enqueue cs
>   zeros = S.filter (hasPort 0) q
>   in (\z -> bridgesFrom z $ S.delete z q) `concatMap` zeros

> bridgesFrom :: Component -> Queue -> [Bridge]
> bridgesFrom c@(C (_,b)) q = let
>   reachable = S.filter (hasPort b) q
>   bs = (liftM2 bridgesFrom (orient c) (flip S.delete q)) `concatMap` reachable
>   in case null reachable of
>     True  -> [[c]]
>     False -> {- [[c]] ++ -} map (c:) bs

The fun bit at ^^^^^^^^^^^^^^ is an optimization. If we are able to continue
building a bridge, we can forget of the shorter ones we end up with on the way
there to its actual end. If one wants to collect them, that's the concatenation
to do. Or ([c]:), sure.

> strength :: Bridge -> Int
> strength = getSum . foldMap (Sum . uncurry (+) . unC)

> main :: IO ()
> main = do
>   bs <- bridges . parse <$> readFile "input.txt"
>
>   print . (strength &&& (length &&& id))
>         . maximumBy (comparing strength) $ bs
>
>   print . (strength &&& (length &&& id))
>         . maximumBy (comparing strength)
>         . head . groupBy ((==) `on` length)
>         . reverse . sortBy (comparing length) $ bs
