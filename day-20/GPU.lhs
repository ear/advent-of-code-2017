Got an idea for a different approach to parsing today :D

> module Main where
> import Data.Ord
> import Data.Char
> import Data.List
> import Data.Function
> import Control.Arrow
> import qualified Data.Map.Strict as M
> import System.Environment (getArgs)

> import Debug.Trace

> type V3 = [Integer]

> newtype Position = Position (V3) deriving (Show, Eq)
> newtype Velocity = Velocity (V3) deriving (Show, Eq)
> newtype Acceleration = Acceleration (V3) deriving (Show, Eq)

> newtype Particle = Particle (Position, Velocity, Acceleration) deriving (Show, Eq)

> position, velocity, acceleration :: Particle -> V3
> position (Particle (Position x, _, _)) = x
> velocity (Particle (_, Velocity v, _)) = v
> acceleration (Particle (_, _, Acceleration a)) = a

> parse :: String -> [Particle]
> parse = map (toParticle . map read . words) . lines . concatMap clean
>   where
>     clean c | isDigit c || elem c "-\n" = [c]
>             | c == ','                  = [' ']
>             | otherwise                 = []
>     toParticle [x,y,z,vx,vy,vz,ax,ay,az]
>       = Particle ( Position [x,y,z]
>                  , Velocity [vx,vy,vz]
>                  , Acceleration [ax,ay,az] )
>     toParticle _ = error "parse error"

> after :: Integer -> Particle -> Particle
> after time particle = let
>   a' = acceleration particle
>   v' = zipWith (+) (velocity particle) (map (time *) a')
>   x' = zipWith (+) (position particle) (map (time *) v')
>   in Particle (Position x', Velocity v', Acceleration a')

> main :: IO ()
> main = do
>   [fileName] <- getArgs
>   particles <- parse <$> readFile fileName
>   -- let futures = map (\epoch -> (epoch, map (after epoch) particles))
>   --                   ([10,20..160] ++ [100000])
>   -- putStrLn "(Epoch,((Concordant,Discordant),(Particle ID,Particle)))"
>   -- mapM_ (print . second (partitionAV &&& closestToOrigin)) $ futures
>   -- mapM_ print . take 10 $ remainingAfterAllCollisions particles
>   let select xs = [ (x,ys) | x <- xs, let ys = xs \\ [x] ]
>   mapM_ (putStr . show) $ map (\((i,p),rest) -> filter (willCollide p) (map snd rest)) $ select $ zip [(0::Int)..] particles

Part 1
======

> closestToOrigin :: [Particle] -> (Int,Particle)
> closestToOrigin = minimumBy (comparing norm1 `on` position . snd) . zip [0..]

> partitionAV :: [Particle] -> (Int,Int)
> partitionAV = (length *** length) . partition accordantVA

> norm1 :: [Integer] -> Integer
> norm1 = sum . map abs

> accordantVA :: Particle -> Bool
> accordantVA = all (>= 0) . uncurry (zipWith (+)) . (velocity &&& acceleration)

Part 2
======

Rational roots theorem with q=1.

> polyFromRoots x1 x2 = [x1*x2,negate (x1+x2),1]

> hasIntegralRoots :: [Integer] -> Bool
> hasIntegralRoots [0,0,0] = True
> hasIntegralRoots [0,a1,a2] = hasIntegralRoots [a1,a2,0]
> hasIntegralRoots [a0,a1,a2] = not . null $ filter (\d -> a0 + a1*d + a2*d*d == 0) (divisors a0)
> hasIntegralRoots _ = error "hasIntegralRoots pattern match"

Collision.

> willCollide :: Particle -> Particle -> Bool
> willCollide
>   (Particle (Position x0, Velocity v0, Acceleration a0))
>   (Particle (Position x1, Velocity v1, Acceleration a1)) = let
>     polynomials = transpose
>                    [zipWith (-) x0 x1, zipWith (-) v0 v1, zipWith (-) a0 a1]
>     in all hasIntegralRoots polynomials

Helpers.

> factors :: Integer -> [Integer]
> factors = f (head primes) (tail primes) where
>   f n ns m
>     | m < 2 = []
>     | m < n ^ (2 :: Integer) = [m]   -- stop early
>     | m `mod` n == 0 = n : f n ns (m `div` n)
>     | otherwise = f (head ns) (tail ns) m

> primes :: [Integer]
> primes = 2 : filter (\n-> head (factors n) == n) [3,5..]

> choose :: [b] -> Int -> [[b]]
> _      `choose` 0 = [[]]
> []     `choose` _ =  []
> (x:xs) `choose` k =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

> divisors :: Integer -> [Integer]
> divisors n
>   | n < 0 = divisors (-n)
>   | otherwise = let
>     fs = factors n
>     ps = nub $ (>>= (map product) . choose fs) [0..length fs]
>     in ps ++ map negate ps

Action.

> remainingAfterAllCollisions :: [Particle] -> [Int]
> remainingAfterAllCollisions = map M.size .
>   iterate (\m -> go (Just . fst . M.findMin $ m) m) . M.fromList . zip [0..]
>     where
>       go :: Maybe Int -> M.Map Int Particle -> M.Map Int Particle
>       go Nothing m = m
>       go (Just i) m = let
>         Just p1 = M.lookup i m
>         collisions = M.foldlWithKey'
>           (\cs j p2 -> if (j /= i) && willCollide p1 p2 then (j:i:cs) else cs)
>           [] m
>         m' = foldl' (flip M.delete) m collisions
>         i' = fst <$> M.lookupGT i m'
>         in go i' m'

 findFirstRepeatingElement = head . head .
   dropWhile (\[a,b] -> a/=b) . map (take 2) . tails





Finding integral solutions to 2nd degrees polynomials.

P(n) = 0 => P(n) mod 2 = 0 mod 2

Hence if P(0)=1=P(1) mod 2, then P(_) doens't have integer solutions.

Let's see how many particles we can get rid of with this!

  print . reverse . sort . map snd .
    map (id &&&
         (\i -> length $ filter (not . cannotCollide (particles !! i)) particles))
    [0 .. pred (length particles)]

Turns out, very few, one particle could collide with 992. The others all more.

> cannotCollide :: Particle -> Particle -> Bool
> cannotCollide
>   (Particle (Position x0, Velocity v0, Acceleration a0))
>   (Particle (Position x1, Velocity v1, Acceleration a1)) =
>     ([1,1,1] == (zipWith (\a b -> mod (a-b) 2) x0 x1))
>     &&
>     ([1,1,1] == zipWith3 (\a b c -> a + b + c)
>                   (zipWith (\a b -> mod (a-b) 2) x0 x1)
>                   (zipWith (\a b -> mod (a-b) 2) v0 v1)
>                   (zipWith (\a b -> mod (a-b) 2) a0 a1))


