> {-# OPTIONS_GHC -Wno-unused-do-bind #-}
> {-# LANGUAGE ViewPatterns #-}
> module Main where

> import Text.Parsec hiding (parse, Line)
> import Text.Parsec.String
> import Data.Char (ord, chr)
> import Data.Vector.Unboxed (Vector)
> import qualified Data.Vector.Unboxed as U
> import Data.Vector.Unboxed.Mutable ()
> import Data.List
> import System.Environment
> import Data.Map (Map)
> import qualified Data.Map as M
> import Debug.Trace

A dance is a sequence of `Move`s.

> newtype Dance = D [Move]
>   deriving Show

There are three possible moves.

> data Move = Spin !Int
>           | Exchange !Int !Int
>           | Partner !Int !Int
>   deriving Show

The programs dance standing in a `Line`.

> newtype Line = L (Vector Int) deriving Eq

Mapping the 16 letters from 'a' to 'p' to zero-based numbers the starting
position becomes:

> initialLine :: Line
> initialLine = L (U.fromList [0..15])

A `Show` instance to show the letters in a `Line`.

> instance Show Line where
>   show (L xs) = map (chr . ((ord 'a') +)) $ U.toList xs

Corresponding `Read` instance to go from a 16 characters string to a `Line`.

> instance Read Line where
>   readsPrec _ (splitAt 16 -> (xs,ys)) =
>     [(L . U.fromList . map ((subtract $ ord 'a') . ord) $ xs, ys)]

An easy way to introduce lines.

> line :: [Int] -> Line
> line = L . U.fromList

Let's introduce the concept of a permutation to capture the effect of dancing
on a given `Line`.

> newtype Perm = P (Vector Int)

> instance Show Perm where
>   show (P xs) = "P:" ++ show xs

Permutations are vectors such that their i-th element contains the position
that the "program" in position i should go to.

e.g. [1,0,2,...,15] is the permutation that swaps the first two "programs" in
a given `Line`.

A useful permutation is the identity (which moves no-one.) Let's call it `one`.

> one :: Perm
> one = P $ U.fromList [0..15]

Other permutations that are needed are rotation by n and swapping two elements.

> rotate :: Int -> Perm
> rotate n = P $ U.fromList [ (i - n) `mod` 16 | i <- [0..15] ]

> swap :: Int -> Int -> Perm
> swap a b = P $ U.fromList [ x | i <- [0..15], let x = swap' i ]
>   where swap' k | k == a = b | k == b = a | otherwise = k

Generic permutation composition.

> fromPerm :: Perm -> Vector Int
> fromPerm (P xs) = xs

> (@@) :: Vector Int -> Vector Int -> Vector Int
> p1 @@ p2 = U.backpermute p1 p2

> step :: Line -> Move -> Line
> step (L xs) (Spin n)       = L $ xs @@ fromPerm (rotate n)
> step (L xs) (Exchange a b) = L $ xs @@ fromPerm (swap a b)
> step (L xs) (Partner a b)  = L $ fromPerm (swap a b) @@ xs

> dance :: Line -> Dance -> Line
> dance l (D ds) = foldl' step l ds

> danceLines :: Dance -> [Line]
> danceLines = scanl' dance initialLine . repeat

findLoop :: [Line] -> Map (Vector Int) Int
findLoop = foldl' go (M.empty :: Map (Vector Int) Int) . zip [0..]
  where
    go m (j,L l) = case M.lookup l m of
      Just i -> traceShow (i,j,l) m
      Nothing -> M.insert l j m

> findLoop :: [Line] -> (Int, Int, Map (Vector Int) Int)
> findLoop = foldl' go (0, 0, M.empty :: Map (Vector Int) Int) . zip [0..]
>   where
>     go (i,j,m) (j',L l) = case M.lookup l m of
>       Just i -> (i,j,m)
>       Nothing -> (i,j,M.insert l j m)

> main :: IO ()
> main = do
>   -- [n] <- map read <$> getArgs
>   d <- parse <$> readFile "input.txt"
>   print $ findLoop $ danceLines d

> danceN :: Int -> Dance -> Line
> danceN = (foldl' dance initialLine .) . replicate

Down to only 69 days of computation this way >_>

main :: IO ()
main = do
  [n] <- map read <$> getArgs
  d <- parse <$> readFile "input.txt"
  print $ danceN n d






A `Dance` can be expressed as a `Perm` which mutates its starting `Line` into
its eventual ending `Line`.

There is one caveat: the permutation is not uniquely determined. This is
because of the `Partner` moves. Here's an example.

                   line 1 |   move      | line 2
                  --------+-------------+--------
                   0 1 2  | Partner 1 2 | 1 0 2
                   0 2 1  | Spin 1      | 2 0 1
                   1 0 2  |             | 1 2 0
 resulting perm:   1 0 2                  0 2 1

Exchange and Spin only care about positions, not contents, hence the resulting
permutation is the same, here's an example:

                   line 1 |   move       | line 2
                  --------+--------------+--------
                   0 1 2  | Exchange 1 2 | 1 0 2
                   0 2 1  | Spin 1       | 1 2 0
                   1 0 2  |              | 0 1 2
 resulting perm:   1 0 2                   1 0 2

If there is any hope of reducing the amount of work of the 1.000.000.000
iterations asked it is in finding a loop in the sequence of `(Line,Perm)` pairs.

In particular if going through `N` `Dance`s starting from a given line comes
back to any of its previous states we can cut down on the amount of computation,
hopefully drastically.


type Memo = Map (Line,Perm) Int



Goal: translate a `Dance` starting from a given line into its `Perm`.

permFromDance :: Line -> Dance -> (Line,Perm)


f :: Line -> Dance -> (Line,Perm)





Parsing

> parse :: String -> Dance
> parse xs = let Right d = runParser p_dance () "input.txt" xs in d

> p_dance :: Parser Dance
> p_dance = D <$> p_move `sepBy1` (char ',')

> p_move :: Parser Move
> p_move = choice [p_spin, p_exchange, p_partner]

> p_number, p_name :: Parser Int
> p_number = read <$> many1 digit
> p_name = (subtract (ord 'a') . ord) <$> oneOf ['a'..'p']

> p_spin, p_exchange, p_partner :: Parser Move
> p_spin = Spin <$> (char 's' >> p_number)
> p_exchange = do
>   { char 'x'; a <- p_number; char '/'; b <- p_number; return $ Exchange a b }
> p_partner = do
>   { char 'p'; a <- p_name; char '/'; b <- p_name; return $ Partner a b }
