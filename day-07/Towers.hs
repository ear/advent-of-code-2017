{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Tree
-- import Data.List
import Data.Monoid
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
import Data.Sequence (ViewL(..), (|>))
import qualified Data.Sequence as S

import Control.Arrow ((***))

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ts = map (toTree . parse . words) . lines $ input
  let tree = merge ts
  putStr . drawTree . fmap (uncurry (++) . ((++ " ") *** show)) $ tree
  writeFile "input.tree" (show tree)

-- Parsing

data Layer = L { name_ :: String, weight_ :: Int, children_ :: [String] }
  deriving (Show)

parse :: [String] -> Layer
parse (x:y:xs) = L x w (map removeCommas . drop 1 $ xs)
  where
    w = read . init . tail $ y
    removeCommas = reverse . dropWhile (',' ==) . reverse
parse _ = error "parse error"

-- Trees

type Tower = Tree (String,Int)

toTree :: Layer -> Tower
toTree (L x w xs) = Node (x,w) (map toTree' xs)
  where
    toTree' :: String -> Tower
    toTree' label = Node (label,-1) []

insertTower :: Tower -> Tower -> (Bool, Tower)
insertTower t@(Node (n1,_) _) (Node l@(n2,_) ts)
  | n1 == n2  = (True, t)
  | otherwise = let (bool, ts') = ((getAny . foldMap Any) *** id) . unzip . map (insertTower t) $ ts
                in (bool, Node l ts')

merge :: [Tower] -> Tower
merge = go . S.fromList
  where
    go (S.viewl -> t :< ts)
      | length ts == 0 = t
      | otherwise = go ts'
        where
          (u,us) = (fmap snd *** fmap snd) . S.partition fst . fmap (insertTower t) $ ts
          ts' | S.null u  = ts |> t
              | otherwise = us |> S.index u 0
    go _ = error "cannot merge 0 towers"

--



--

tName :: Tower -> String
tName = fst . rootLabel
tWeight :: Tower -> Int
tWeight = snd . rootLabel
tChildren :: Tower -> [Tower]
tChildren = subForest
isLeaf :: Tower -> Bool
isLeaf = null . subForest

