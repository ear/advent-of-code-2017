{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE BangPatterns, LambdaCase #-}
module Main where

import Perm

import Prelude hiding (floor)

import Data.Char (ord, chr)
import Text.Parsec hiding (parse)
import Text.Parsec.String

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.List (foldl', foldl1', find)
import Control.Arrow

import Debug.Trace

main :: IO ()
-- main = putStrLn . toString . dance . parse =<< readFile "input.txt"
main = do
  d <- parse <$> readFile "input.txt"
  -- putStrLn . toString $ danceN d 1000000000
  -- print $ f (danceAsPerm d) 21
  let dp = danceAsPerm d
  let effectiveExponent = 1000000000 `rem` period dp
  print effectiveExponent
  print . dance begin . power dp $ effectiveExponent

newtype Dance = D [Move] deriving Show
data Move = Spin !Int | Exchange !Int !Int | Partner !Int !Int deriving Show

newtype Floor = F (Map Int Int) deriving Show

danceAsPerm :: Dance -> Perm
danceAsPerm (D ds) = foldl1' apply . snd . foldl' go (begin, []) $ ds
  where
    go (floor, ps) move = (floor', p:ps)
      where
        (floor', p) = traceSteps floor move

dance :: Floor -> Perm -> String
dance (F m) = toString . F . M.fromList . listFromPerm . apply (permFromList . M.elems $ m)

traceSteps :: Floor -> Move -> (Floor,Perm)
traceSteps (F m) (Spin n) = let
  p = permFromPairs . map (\k -> (k, (n+k) `mod` 16)) $ M.keys m
  in (F $ M.mapKeys ((`mod` 16) . (n+)) m, p)
traceSteps (F m) (Exchange ia ib) = let
  Just !a = M.lookup ia m
  Just !b = M.lookup ib m
  m' = M.update (const $ Just a) ib . M.update (const $ Just b) ia $ m
  p = permFromPairs [(ia,ib),(ib,ia)]
  in (F m', p)
traceSteps (F m) (Partner a b) = let
  go !acc@(!_ia,!_ib) !i !v
    | v == a = (i  ,_ib)
    | v == b = (_ia,i  )
    | otherwise = acc
  (ia,ib) = M.foldlWithKey' go (0,0) m
  m' = M.update (const $ Just a) ib . M.update (const $ Just b) ia $ m
  p = permFromPairs [(ia,ib),(ib,ia)]
  in (F m', p)

begin :: Floor
begin = F $ M.fromList [(i,i) | i <- [0..15]]

toString :: Floor -> String
toString (F m) = map (chr . ((ord 'a') +)) . M.elems $ m

permFloor :: Perm -> String
permFloor = map (chr . ((ord 'a') +)) . permAsList

listFloor :: [Int] -> String
listFloor = map (chr . ((ord 'a') +))

-- Parsing

parse :: String -> Dance
parse xs = let Right d = runParser pDance () "input.txt" xs in d

pDance :: Parser Dance
pDance = D <$> pMove `sepBy1` (char ',')

pMove :: Parser Move
pMove = choice [pSpin, pExchange, pPartner]

p_number, p_name :: Parser Int
p_number = read <$> many1 digit
p_name = (subtract (ord 'a') . ord) <$> oneOf ['a'..'p']

pSpin, pExchange, pPartner :: Parser Move
pSpin = Spin <$> (char 's' >> p_number)
pExchange = do
  { char 'x'; a <- p_number; char '/'; b <- p_number; return $ Exchange a b }
pPartner = do
  { char 'p'; a <- p_name; char '/'; b <- p_name; return $ Partner a b }

-- Test data

t1 :: Dance
t1 = parse "s1,x3/4,pe/b"
