{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Sequence (Seq, ViewL(..), (><))
import qualified Data.Sequence as S

import Control.Arrow ((***), (&&&))

-- import Data.Tree (Tree(..))
-- import qualified Data.Tree as T

main :: IO ()
main = do
  input <- head . lines <$> readFile "input.txt"
  let xs = S.fromList input
  print . score . uncomma . ungarbage . unescape $ xs

type Stream = Seq Char

-- eat '!.' at the beginning of a stream
eatEscape :: Stream -> Stream
eatEscape (S.viewl -> '!' :< (S.viewl -> _ :< rest)) = rest
eatEscape xs = xs

-- eat 1 escaped pair and return the resulting pair of sequences
unescape1 :: (Stream, Stream) -> (Stream, Stream)
unescape1 (rest, xs) = ((rest ><) *** eatEscape) . S.breakl ('!'==) $ xs

-- eat all escaped pairs from a stream
unescape :: Stream -> Stream
unescape = fst . head . dropWhile (not . S.null . snd) . iterate unescape1 . (S.empty,)

-- eat garbage at the beginning of a stream
eatGarbage :: Stream -> Stream
eatGarbage (S.viewl -> '<' :< rest) = S.drop 1 . snd . S.breakl ('>' ==) $ rest
eatGarbage xs = xs

-- eat 1 instance of garbage and return the resulting pair of sequences
ungarbage1 :: (Stream, Stream) -> (Stream, Stream)
ungarbage1 (rest, xs) = ((rest ><) *** eatGarbage) . S.breakl ('<'==) $ xs

-- eat all instances of garbage from a stream
ungarbage :: Stream -> Stream
ungarbage = fst . head . dropWhile (not . S.null . snd) . iterate ungarbage1 . (S.empty,)

-- eat all commas from a stream
uncomma :: Stream -> Stream
uncomma = (>>= dropComma)

dropComma :: Char -> Stream
dropComma ',' = S.empty
dropComma  x  = S.singleton x

-- score a stream
score :: Stream -> Int
score = sumScore . uncurry (S.zipWith (*)) . ((fmap chi_1) &&& (S.scanl (+) 0)) . fmap parenToInt
  where
    chi_1 1 = 1
    chi_1 _ = 0
    parenToInt '{' = 1
    parenToInt '}' = -1
    parenToInt  _  = error "stream contains other characters than '(' and ')'"
    sumScore = foldl (+) 1 . fmap succ . (>>= dropZero)
    dropZero 0 = S.empty
    dropZero x = S.singleton x

tests :: IO ()
tests = do
  input <- readFile "tests.txt"
  let ts = map ((id &&& score) . uncomma . ungarbage . unescape . S.fromList) . lines $ input
  mapM_ print ts


-- Idea: score a stream transforming it to a Tree, the solution does work
-- but the transformation is hard to do! :D
--
-- stream to tree
-- toTree :: Stream -> Tree Int
-- toTree = undefined
--
-- score a stream
-- score = sum . concat . zipWith (map . const) [1..] . T.levels . toTree

-- Removing "unuseful commas" doesn't make any sense, have to remove all commas
--
-- -- eat the first unbalanced comma and return the resulting pair of sequences
-- unComma1 :: (Stream,Stream) -> (Stream,Stream)
-- unComma1 (past, xs)
--   = case ls of
--       (S.viewl -> '{' :<      (S.viewl -> ',' :< rest)) -> (past' |> '{', rest   )
--       (S.viewl -> ',' :< rest@(S.viewl -> '{' :< _   )) -> (past' |> ',', rest   )
--       (S.viewl -> ',' :< rest@(S.viewl -> ',' :< _   )) -> (past'       , rest   )
--       (S.viewl -> ',' :< rest@(S.viewl -> '}' :< _   )) -> (past'       , rest   )
--       (S.viewl ->  x  :< rest                         ) -> (past' |> x  , rest   )
--       (S.viewl -> EmptyL                              ) -> (past'       , S.empty)
--     where
--       (rs,ls) = S.breakl (\x -> ','==x || '{' == x) xs
--       past' = past >< rs
--
-- -- eat all instances of unbalanced commas from a stream
-- unComma :: Stream -> Stream
-- unComma = fst . head . dropWhile (not . S.null . snd) . iterate unComma1 . (S.empty,)
