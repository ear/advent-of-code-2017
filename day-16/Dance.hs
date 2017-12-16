{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import Data.Char (ord, chr)
import Text.Parsec hiding (parse)
import Text.Parsec.String

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.List (foldl')

main :: IO ()
main = putStrLn . toString . dance . parse =<< readFile "input.txt"

newtype Dance = D [Move] deriving Show
data Move = Spin Int | Exchange Int Int | Partner Int Int deriving Show

newtype Floor = F (Map Int Int) deriving Show

begin :: Floor
begin = F $ M.fromList [(i,i) | i <- [0..15]]

step :: Floor -> Move -> Floor
step (F m) (Spin n) = F $ M.mapKeys ((`mod` 16) . (n+)) m
step (F m) (Exchange ia ib) = let
  Just a = M.lookup ia m
  Just b = M.lookup ib m
  m' = M.update (const $ Just a) ib . M.update (const $ Just b) ia $ m
  in F m'
step (F m) (Partner a b) = let
  go acc@(_ia,_ib) i v
    | v == a = (i  ,_ib)
    | v == b = (_ia,i  )
    | otherwise = acc
  (ia,ib) = M.foldlWithKey' go (undefined,undefined) m
  m' = M.update (const $ Just a) ib . M.update (const $ Just b) ia $ m
  in F m'

dance :: Dance -> Floor
dance (D moves) = foldl' step begin moves

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
