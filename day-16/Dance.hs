{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE BangPatterns, LambdaCase #-}
module Main where

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
  print $ f (danceAsPerm d) 21

newtype Dance = D [Move] deriving Show
data Move = Spin !Int | Exchange !Int !Int | Partner !Int !Int deriving Show

newtype Floor = F (Map Int Int) deriving Show

newtype Perm = P [(Int,Int)] deriving Show

traceSteps :: Floor -> Move -> (Floor,Perm)
traceSteps (F m) (Spin n) = let
  perm = map (\k -> (k, (n+k) `mod` 16)) $ M.keys m
  in (F $ M.mapKeys ((`mod` 16) . (n+)) m, P perm)
traceSteps (F m) (Exchange ia ib) = let
  Just !a = M.lookup ia m
  Just !b = M.lookup ib m
  m' = M.update (const $ Just a) ib . M.update (const $ Just b) ia $ m
  in (F m', P [(ia,ib),(ib,ia)])
traceSteps (F m) (Partner a b) = let
  go !acc@(!_ia,!_ib) !i !v
    | v == a = (i  ,_ib)
    | v == b = (_ia,i  )
    | otherwise = acc
  (ia,ib) = M.foldlWithKey' go (0,0) m
  m' = M.update (const $ Just a) ib . M.update (const $ Just b) ia $ m
  in (F m', P [(ia,ib),(ib,ia)])

comp :: [Perm] -> Perm
comp = foldl1' f
  where
    f :: Perm -> Perm -> Perm
    f (P prev) next = P $ map (g next) prev
    g :: Perm -> (Int,Int) -> (Int,Int)
    g (P next) p@(i,j) =
      case find ((j==) . fst) next of
        Just (_,j') -> (i,j')
        Nothing     -> p

permId = P [(i,i) | i <- [0..15]]

danceAsPerm :: Dance -> Perm
danceAsPerm (D ds) = comp . reverse . snd $ foldl' go (begin, [permId]) ds
  where
    go (floor, ps) move =
      let (floor', p) = traceSteps floor move
      in (floor', p:ps)

{-
p^100 000 000
(p^2)^(50 000 000)
((p^2)^2)^(25 000 000)
-}

square :: Perm -> Perm
square p = comp [p,p]

f :: Perm -> Int -> Perm
f p 1 = p
f p n = f (square p) (n-1)

lastDance :: Perm -> Floor
lastDance (P ps) = let
  theVeryLastDance :: Dance
  theVeryLastDance = D $ map (\(i,j) -> Exchange i j) ps
  in dance theVeryLastDance begin

-- applyPermToFloor floor (P ps) = foldl' go floor ps
--   where
--     go :: Floor -> (Int,Int) -> Floor
--     go (F m) (i,j) = let
--       Just !a = M.lookup i m
--       Just !b = M.lookup j m
--       m' = M.update (const $ Just a) ib . M.update (const $ Just b) ia $ m

begin :: Floor
begin = F $ M.fromList [(i,i) | i <- [0..15]]

step :: Floor -> Move -> Floor
step (F m) (Spin n) = F $ M.mapKeys ((`mod` 16) . (n+)) m
step (F m) (Exchange ia ib) = let
  Just !a = M.lookup ia m
  Just !b = M.lookup ib m
  m' = M.update (const $ Just a) ib . M.update (const $ Just b) ia $ m
  in F m'
step (F m) (Partner a b) = let
  go !acc@(!_ia,!_ib) !i !v
    | v == a = (i  ,_ib)
    | v == b = (_ia,i  )
    | otherwise = acc
  (ia,ib) = M.foldlWithKey' go (0,0) m
  m' = M.update (const $ Just a) ib . M.update (const $ Just b) ia $ m
  in F m'

dance :: Dance -> Floor -> Floor
dance (D moves) floor = foldl' step floor moves

danceN :: Dance -> Int -> Floor
danceN (D moves) n = danceN' moves n begin
  where
    danceN' _   0 floor = floor
    danceN' ms !k floor = danceN' ms (every 100 (k-1)) (foldl' step floor ms)

every !n !k | k `mod` n == 0 = traceShowId k
            | otherwise      = k

toString :: Floor -> String
toString (F m) = map (chr . ((ord 'a') +)) . M.elems $ m

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
