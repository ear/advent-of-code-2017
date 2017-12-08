{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  input <- readFile "input.txt"
  let program = map (parse . words) . lines $ input
      mem :: Memory = M.fromAscList $ zip (registers program) (repeat 0)
  putStrLn . concat $ ["Program: ", show . length $ program, " instructions"]
  putStrLn $ concat [show $ M.size mem, " registers: "] ++ intercalate ", " (M.keys mem)
  let mem' = scanl exec mem program
  putStr "Maximum register value at the end of execution: "
  print . maximum . M.elems . last $ mem'
  putStr "Maximum register value realized during the whole runtime: "
  print . maximum . concatMap M.elems $ mem'

data OP = OP String OPCode Int String Comp Int
  deriving Show

data OPCode = Inc | Dec deriving Show
data Comp   = LT_ | GT_ | LTE_ | GTE_ | EQ_ | NEQ_ deriving Show

parseOPCode :: String -> OPCode
parseOPCode "inc" = Inc
parseOPCode "dec" = Dec
parseOPCode _     = error "unsupported opcode"

parseComp :: String -> Comp
parseComp "<"  = LT_
parseComp "<=" = LTE_
parseComp ">"  = GT_
parseComp ">=" = GTE_
parseComp "==" = EQ_
parseComp "!=" = NEQ_
parseComp _    = error "unsupported comparison"

parse :: [String] -> OP
parse [r1,parseOPCode -> op,read @Int -> n1,_,r2,parseComp -> co,read @Int -> n2]
  = OP r1 op n1 r2 co n2
parse _ = error "malformed program"

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
    update r1' | r2' `comp_` n2 = r1' `op_` n1
               | otherwise      = r1'
    comp_ = compFun comp
    op_   = opFun op

opFun :: OPCode -> (Int -> Int -> Int)
opFun Inc = (+)
opFun Dec = (-)

compFun :: Comp -> (Int -> Int -> Bool)
compFun LT_  = (<)
compFun LTE_ = (<=)
compFun GT_  = (>)
compFun GTE_ = (>=)
compFun EQ_  = (==)
compFun NEQ_ = (/=)
