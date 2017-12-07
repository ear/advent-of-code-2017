{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Tree
import Data.List
import Data.Maybe
import Data.Monoid
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
import Data.Sequence (ViewL(..), (|>))
import qualified Data.Sequence as S

import Control.Arrow ((&&&), (***))

main :: IO ()
main = do
  input <- readFile "test.tree"
  let tree = read @Tower input
  putStr . drawTree . fmap (uncurry (++) . ((++ " ") *** show)) $ tree
  mapM_ print $ equations tree

readTower name = read @Tower <$> readFile name

-- Trees

type Tower = Tree (String,Int)

data Equation = E String Int [Equation]

instance Show Equation where
  show e = showVar e ++ " = " ++ showNum e ++ " = " ++ show (sumEq e)

showVar (E x w []) = x
showVar (E x w es) = x ++ " + " ++
                        ("(" ++
                        intercalate " + " (map showVar es)
                        ++ ")")

showNum (E x w []) = show w
showNum (E x w es) = show w ++ " + " ++
                        ("(" ++
                        intercalate " + " (map showNum es)
                        ++ ")")

sumEq (E _ w []) = w
sumEq (E _ w es) = w + sum (map sumEq es)

equations :: Tower -> [Equation]
equations (Node (l,w) ts) = map equations' ts

equations' :: Tower -> Equation
equations' (Node (l,w) []) = E l w []
equations' (Node (l,w) ts) = E l w $ map equations' ts

toTree :: Equation -> Tree (String,Int)
toTree (E x w es) = Node (x,w) $ map toTree es

sums :: Tower -> [(String,Int)]
sums = map (eqLabel &&& sumEq) . equations

eqLabel (E l _ _) = l

(-$) :: Tower -> String -> Tower
(-$) (Node _ ts) l = fromJust . find ((l ==) . tLabel) $ ts

tLabel (Node (l,_) _) = l

weights (Node _ ts) = map tWeight ts

tWeight (Node (_,w) _) = w

childrenCount :: Tower -> Tree Int
childrenCount (Node _ []) = Node 0 []
childrenCount (Node _ ts) = Node (length ts) $ map childrenCount ts

p :: Tower -> IO ()
p = putStr . drawTree . fmap (\(x,w) -> x ++ " " ++ show w)
