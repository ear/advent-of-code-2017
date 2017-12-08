{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main = do
  input <- readFile "input.txt"
  let program = map (parse . words) . lines $ input
      mem :: Memory = M.fromAscList $ zip (registers program) (repeat 0)
  mapM_ print program
  putStrLn $ concat [show $ M.size mem, " registers: "] ++ intercalate ", " (M.keys mem)
  print mem
  let mem' = scanl exec mem program
  -- mapM_ print mem'
  print . maximum . M.elems . last $ mem'
  print . maximum . concatMap M.elems $ mem'

data OP = OP String OPCode Int String Comp Int
  deriving Show

data OPCode = Inc | Dec deriving Show
data Comp   = LT_ | GT_ | LTE_ | GTE_ | EQ_ | NEQ_ deriving Show

parseOPCode "inc" = Inc
parseOPCode "dec" = Dec

parseComp "<"  = LT_
parseComp "<=" = LTE_
parseComp ">"  = GT_
parseComp ">=" = GTE_
parseComp "==" = EQ_
parseComp "!=" = NEQ_

parse :: [String] -> OP
parse [r1,parseOPCode -> op,read @Int -> n1,_,r2,parseComp -> co,read @Int -> n2]
  = OP r1 op n1 r2 co n2

type Program = [OP]

registers :: Program -> [String]
registers = sort . (>>= extractRegs)
  where
    extractRegs (OP r1 _ _ r2 _ _) = [r1,r2]

type Memory = Map String Int

run :: Memory -> Program -> Memory
run = foldl' exec

exec :: Memory -> OP -> Memory
exec m (OP r1 op n1 r2 comp n2) = M.adjust update r1 m
  where
    Just r2' = M.lookup r2 m
    update r1 | r2' `comp_` n2 = r1 `op_` n1
              | otherwise      = r1
    comp_ = compFun comp
    op_   = opFun op

opFun Inc = (+)
opFun Dec = (-)

compFun LT_  = (<)
compFun LTE_ = (<=)
compFun GT_  = (>)
compFun GTE_ = (>=)
compFun EQ_  = (==)
compFun NEQ_ = (/=)
