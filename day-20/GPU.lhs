Got an idea for a different approach to parsing today :D

> module Main where
> import Data.Ord
> import Data.Char
> import Data.List
> import Data.Function
> import qualified Data.Vector.Unboxed as V
> import Control.Arrow

> type V3 = [Integer]

> newtype Position = Position (V3) deriving (Show)
> newtype Velocity = Velocity (V3) deriving (Show)
> newtype Acceleration = Acceleration (V3) deriving (Show)

> newtype Particle = Particle (Position, Velocity, Acceleration) deriving (Show)

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
>   particles <- parse <$> readFile "input.txt"
>   let futures = map (\epoch -> (epoch, map (after epoch) particles))
>                     ([10,20..160] ++ [100000])
>   putStrLn "(Epoch,((Concordant,Discordant),(Particle ID,Particle)))"
>   mapM_ (print . second (partitionAV &&& closestToOrigin)) $ futures


> closestToOrigin = minimumBy (comparing norm1 `on` position . snd) . zip [0..]

> partitionAV = (length *** length) . partition accordantVA

> norm1 :: [Integer] -> Integer
> norm1 = sum . map abs

> accordantVA :: Particle -> Bool
> accordantVA = all (>= 0) . uncurry (zipWith (+)) . (velocity &&& acceleration)
