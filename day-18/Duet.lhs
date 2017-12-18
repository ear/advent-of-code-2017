> module Main where

> import Data.List
> import Control.Monad
> import Text.Parsec        hiding (State)
> import Text.Parsec.Char
> import Text.Parsec.String

> import Data.Maybe
> import Data.Foldable
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict as M
> import Control.Monad.Trans.State.Strict

> data Lit
>   = L Char           -- Labels: 'a', 'b', ...
>   | N Int            -- Numbers: 3, -9, ...
>   deriving (Show)

> data OP
>   = SND Lit          -- snd X    :  play frequency X
>   | SET Lit Lit      -- set X Y  :  X = Y
>   | ADD Lit Lit      -- add X Y  :  X += Y
>   | MUL Lit Lit      -- mul X Y  :  X *= Y
>   | MOD Lit Lit      -- mod X Y  :  X %= Y
>   | RCV Lit          -- rcv X    :  retrieve last frequency played
>   | JGZ Lit Lit      -- jgz X Y  :  if (X>0) jump by Y
>   deriving (Show)

> number :: Parser Int
> number = read <$> ((:) <$> (option ' ' (char '-')) <*> many1 digit)

read ' 12' => 12 but maybe there's a simpler way to have '-' prepended if it is
there otherwise have nothing done to many1 digit.

> lit :: Parser Lit
> lit = spaces >> (try (L <$> letter) <|> (N <$> number))

> type Program = [OP]

> parseProgram :: Parser Program
> parseProgram =
>   flip sepBy1 (char '\n') $ do
>     op <- replicateM 3 letter
>     case op of
>       "snd" -> SND <$> lit
>       "set" -> SET <$> lit <*> lit
>       "add" -> ADD <$> lit <*> lit
>       "mul" -> MUL <$> lit <*> lit
>       "mod" -> MOD <$> lit <*> lit
>       "rcv" -> RCV <$> lit
>       "jgz" -> JGZ <$> lit <*> lit

> data S = S { regs_ :: Map Char Int, played_ :: [Int], recovered_ :: [Int] }
>   deriving (Show)

> emptyS :: S
> emptyS = S { regs_ = M.empty, played_ = [], recovered_ = [] }

> type Eval = State S

> eval :: Program -> S
> eval p = execState (evalProgram p) emptyS

> evalProgram :: Program -> Eval ()
> evalProgram = traverse_ evalOP

`v` extracts the value out of a literal, two cases:
  - the current value of the named register
  - the specified number

> v :: Lit -> Eval Int
> v (L x) = (fromMaybe 0 . M.lookup x) <$> gets regs_
> v (N x) = pure x

> evalOP :: OP -> Eval ()
> evalOP (SND l1      ) = _snd =<< v l1
> evalOP (SET (L x) l2) = _set x =<< v l2
> evalOP (ADD (L x) l2) = _op2 (+)   x =<< v l2
> evalOP (MUL (L x) l2) = _op2 (*)   x =<< v l2
> evalOP (MOD (L x) l2) = _op2 (mod) x =<< v l2
> evalOP (RCV (L x)   ) = _rcv x
> evalOP (JGZ (L x) l2) = _jgz x =<< v l2

> _snd :: Int -> Eval ()
> _snd freq = modify (\s -> s { played_ = freq : played_ s })

> _set :: Char -> Int -> Eval ()
> _set reg val = modify (\s ->
>   s { regs_ = M.alter (Just . const val) reg $ regs_ s })

> _op2 :: (Int -> Int -> Int) -> Char -> Int -> Eval ()
> _op2 (@@) reg val = modify (\s ->
>   s { regs_ = M.adjust (@@ val) reg $ regs_ s })

> _rcv :: Char -> Eval ()
> _rcv reg = do
>   Just x <- M.lookup reg <$> gets regs_
>   when (x > 0) $ modify (\s ->
>     s { recovered_ = head (played_ s) : (recovered_ s) })

> _jgz :: Char -> Int -> Eval ()
> _jgz reg offset = do
>   Just x <- M.lookup reg <$> gets regs_
>   when (x >= 2) $ undefined

> main :: IO ()
> main = do
>   Right program <- parse parseProgram "input.txt" <$> readFile "input.txt"
>   mapM_ print program
