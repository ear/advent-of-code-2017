> import qualified Data.Set as S
> import Data.Ord
> import Data.List
> import Data.Monoid
> import Data.Foldable
> import Control.Arrow

> newtype Component = C (Int,Int) deriving (Ord, Eq)

> instance Show Component where show (C (a,b)) = show a ++ '/' : show b

> unC :: Component -> (Int,Int)
> unC (C c) = c

> hasPort :: Int -> Component -> Bool
> hasPort n = uncurry (||) . ((n==) *** (n==)) . unC

this is pretty… alas not useful:
connectsTo :: Component -> Component -> Bool
connectsTo (C (a,b)) = (||) <$> hasPort a <*> hasPort b

> parse :: String -> [Component]
> parse = map C
>       . sort . map (sortPair . second (read . tail) . head . reads) . lines
>   where sortPair (x,y) | x > y = (y,x) | otherwise = (x,y)

Operating under the assumption that there are no duplicates. It is true for both
the test and the given input. Gonna need a MultiSet otherwise.

> newtype Queue = Q (S.Set Component) deriving (Show)

> unQ :: Queue -> S.Set Component
> unQ (Q s) = s

> enqueue :: [Component] -> Queue
> enqueue = Q . S.fromList

> type Bridge = [Component]

> bridges :: [Component] -> [Bridge]
> bridges cs = let
>   q = enqueue cs
>   zeros = S.filter (hasPort 0) $ unQ q
>   in concatMap (\z -> bridgesFrom z $ Q $ S.delete z $ unQ q) zeros

> bridgesFrom :: Component -> Queue -> [Bridge]
> bridgesFrom c@(C (_,b)) (Q s)
>   = let
>     reachable = toList $ S.filter (hasPort b) s
>     orient (C (x,y)) | x == b = C (x,y) | otherwise = C (y,x)
>     bridges' = concatMap (\r -> bridgesFrom (orient r) $ Q $ S.delete r s) reachable
>     in case null reachable of
>       True -> [[c]]
>       False -> {- [[c]] ++ -} map (c:) bridges'

> strength :: Bridge -> Int
> strength = getSum . foldMap (Sum . uncurry (+) . unC)

> main :: IO ()
> main = print =<< (strength &&& id) . maximumBy (comparing strength) . bridges . parse <$> readFile "input.txt"
