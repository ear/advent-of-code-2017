{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

> module Main where
> import Data.Ord
> import Data.Char
> import Data.List
> import Data.Maybe
> import Data.Function
> import Control.Arrow
> import qualified Data.Map.Strict as M
> import System.Environment (getArgs)

> import Debug.Trace

> main :: IO ()
> main = do
>   ps <- parse <$> readFile "input.txt"
>   print . length . head . drop 1 . survivors $ ps

Vectors as triples. Integers for safety.

> type V3 = (Integer,Integer,Integer)

> x_, y_, z_ :: V3 -> Integer
> x_ (x,_,_) = x
> y_ (_,y,_) = y
> z_ (_,_,z) = z

> (@+) :: V3 -> V3 -> V3
> (x0,y0,z0) @+ (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)

> (@*) :: Integer -> V3 -> V3
> a @* (x,y,z) = (a*x,a*y,a*z)

Particles.

> newtype Particle = P (V3,V3,V3) deriving (Show, Eq)

> p, v, a :: Particle -> V3
> p (P (x,_,_)) = x
> v (P (_,v,_)) = v
> a (P (_,_,a)) = a

A to-the-point approach to parsing 😜

> parse :: String -> [Particle]
> parse = map (toParticle . map read . words) . lines . concatMap clean
>   where
>     clean c | isDigit c || elem c "-\n" = [c]
>             | c == ','                  = [' ']
>             | otherwise                 = []
>     toParticle [x,y,z,vx,vy,vz,ax,ay,az] = P ((x,y,z), (vx,vy,vz), (ax,ay,az))
>     toParticle _ = error "parse error"

To find collisions between two particles we solve integral-coefficient
polynomials for integral positive roots, then intersect their solutions.

> collisions :: Particle -> Particle -> Solutions
> collisions p0@(P (x0,v0,a0)) p1@(P (x1,v1,a1)) = let
>   (x0x,x0y,x0z) = x0; (x1x,x1y,x1z) = x1
>   (v0x,v0y,v0z) = v0; (v1x,v1y,v1z) = v1
>   (a0x,a0y,a0z) = a0; (a1x,a1y,a1z) = a1
>   -- the calculation of the position is funky!
>   px = (2*(x0x-x1x), 2*(v0x-v1x) + a0x-a1x, a0x-a1x)
>   py = (2*(x0y-x1y), 2*(v0y-v1y) + a0y-a1y, a0y-a1y)
>   pz = (2*(x0z-x1z), 2*(v0z-v1z) + a0z-a1z, a0z-a1z)
>   roots = map positiveRoots [px,py,pz]
>   -- foo | (length . filter nonZero $ roots) == 3 = traceShowId (roots,p0,p1)
>   --     | otherwise = ([],P (undefined,undefined,undefined), P (undefined,undefined,undefined))
>   -- in foo `seq` foldl' (∩) Infinite roots
>   in  foldl' (∩) Infinite roots

> data Solutions = Zero | N [Integer] | Infinite deriving (Show, Eq)

> nonZero :: Solutions -> Bool
> nonZero Zero = False
> nonZero _    = True

Let's find the positive roots of an integer-coefficients polynomial of the form:

    a0 + a1*x + a2*x*x

Encoded as a vector (a0,a1,a2):

> positiveRoots :: V3 -> Solutions
> positiveRoots ( 0, 0, 0) = Infinite
> positiveRoots ( 0,a1,a2) = positiveRoots (a1,a2,0)
> positiveRoots (a0,a1,a2) =
>   case nub $ filter (\x -> a0+(a1*x)+(a2*x*x) == 0) (divisors a0) of
>     [] -> Zero
>     xs -> N xs

> divisors :: Integer -> [Integer]
> divisors n
>   | n < 0 = divisors (negate n)
>   | otherwise = ds ++ map negate ds
>     where
>       ds = (1:) $ (n:) $ nub $ concat [[x, div n x] | x <- [2 .. limit], rem n x == 0]
>       limit = floor $ sqrt $ fromInteger n

> polyFromRoots x1 x2 = (x1*x2,negate (x1+x2),1)

Now to find the common solutions to three polynomials.

> (∩) :: Solutions -> Solutions -> Solutions
> Zero     ∩ _        = Zero
> _        ∩ Zero     = Zero
> Infinite ∩ s        = s
> s        ∩ Infinite = s
> (N xs)   ∩ (N ys)   = N $ intersect xs ys

Now let's simulate the universe and remove particles that collide. This function
will return the number of eventual survivors.

> survivors :: [Particle] -> [M.Map Int Particle]
> survivors = iterate remove . M.fromList . zip [0..]

> remove :: M.Map Int Particle -> M.Map Int Particle
> remove m = let
>   (i,p0) = M.findMin m
>   in go i p0 m
>   where
>     go i p0 m = let
>       f m' j p1 = case p0 /= p1 && nonZero (collisions p0 p1) of
>                     True -> M.delete j (M.delete i m')
>                     False -> m'
>       m' = M.foldlWithKey f m m
>       in case M.lookupGT i m' of
>            Nothing       -> m'
>            Just (i',p0') -> go i' p0' m'
