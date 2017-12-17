This is the solution to part 2.

> {-# OPTIONS_GHC -Wno-unused-do-bind #-}
> {-# LANGUAGE ViewPatterns #-}
> module Main where

> import Data.List
> import Data.Char (ord, chr)
> import Data.Maybe (fromJust)

> import Text.Parsec hiding (parse, Line)
> import Text.Parsec.String

> import Data.Vector.Unboxed (Vector)
> import qualified Data.Vector.Unboxed as U

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

One step in a dance. Notice how `Exchange` and `Partner` are actually right
and left composition. That is quite neat.

> step :: Line -> Move -> Line
> step (L xs) (Spin n)       = L $ xs @@ fromPerm (rotate n)
> step (L xs) (Exchange a b) = L $ xs @@ fromPerm (swap a b)
> step (L xs) (Partner a b)  = L $ fromPerm (swap a b) @@ xs

A single dance.

> dance :: Line -> Dance -> Line
> dance l (D ds) = foldl' step l ds

The lines after the same dance is performed from the previous dance's end
position.

> danceLines :: Dance -> [Line]
> danceLines = scanl' dance initialLine . repeat

Find the first time a line repeats. Which one we search for doesn't matter.

> loopLength :: Eq a => [a] -> Int
> loopLength (x:xs) = succ . fromJust . findIndex (x ==) $ xs
> loopLength _ = error "cannot find loop for list of less than 2 elements"

> main :: IO ()
> main = do
>   d <- parse <$> readFile "input.txt"
>   let ls = danceLines d
>   let n = loopLength ls
>   let (q,r) = 1000000000 `divMod` n
>   putStr . concat $
>     [ "loop length: ", show n, "\n"
>     , "1000000000 = ", show q, "*", show n, " + ", show r, "\n"
>     , "line after ", show r, " dances: " ]
>   print $ ls !! r

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
