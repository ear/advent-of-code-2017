Got an idea for a different approach to parsing today :D

> module Main where
> import Data.Ord
> import Data.Char
> import Data.List
> import Data.Function
> import Control.Arrow
> import System.Environment (getArgs)

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

Eyeball some futures. Find a reasonable answer, hopefully it's correct!

> main :: IO ()
> main = do
>   [fileName] <- getArgs
>   particles <- parse <$> readFile fileName
>   let futures = map (\epoch -> (epoch, map (after epoch) particles))
>                     ([10,20..160] ++ [100000])
>   let tuples = map (second (partitionAV &&& closestToOrigin)) $ futures
>   putStrLn "(Epoch,((Concordant,Discordant),(Particle ID,Particle)))"
>   mapM_ print tuples
>   putStr "Particle that remains closest to the origin: "
>   print . snd . snd . last $ tuples

> closestToOrigin :: [Particle] -> (Int,Particle)
> closestToOrigin = minimumBy (comparing norm1 `on` position . snd) . zip [0..]

> partitionAV :: [Particle] -> (Int,Int)
> partitionAV = (length *** length) . partition accordantVA

> norm1 :: [Integer] -> Integer
> norm1 = sum . map abs

> accordantVA :: Particle -> Bool
> accordantVA = all (>= 0) . uncurry (zipWith (+)) . (velocity &&& acceleration)
