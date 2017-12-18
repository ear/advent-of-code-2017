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
> import Control.Monad.Trans.State.Lazy

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

A program "zipper" that focuses on an instruction. Will need this for jumping.

> data Program' = P' { left_ :: [OP], curr_ :: OP, right_ :: [OP] }
>   deriving (Show)

> zipProgram :: Program -> Program'
> zipProgram (o:os) = P' [] o os
> zipProgram _ = error "empty program"

> jump :: Int -> Program' -> Program'
> jump 0 p' = p'
> jump 1    (P' ls x (r:rs)) = (P' (x:ls) r rs)
> jump (-1) (P' (l:ls) x rs) = (P' ls l (x:rs))
> jump n p' | n > 0 = jump (n-1) (jump 1    p')
> jump n p' | n < 0 = jump (n+1) (jump (-1) p')

The machine state `S` for the program evaluation.

> data S = S
>   { prog'_ :: Program'    -- Zipper to keep track of the program execution
>   , regs_ :: Map Char Int -- Map of register names to register values
>   , played_ :: [Int]      -- (Reversed) history of played frequencies
>   , recovered_ :: [Int]   -- (Reversed) history of recovered frequencies
>   } deriving (Show)

> fromProgram :: Program -> S
> fromProgram p = S
>   { prog'_ = zipProgram p, regs_ = M.empty, played_ = [], recovered_ = [] }

> type Eval = State S

Tracing the execution of a program means executing each of its operations
ad infinitum and collecting the sequence of states it puts the machine into.

> trace :: Program -> [S]
> trace = evalState loop . fromProgram

Here we use `sequence` over an infinite list. That is why we need the lazy
version of the `State` monad. The strictness of `(>>=)` would make it loop
forever otherwise.

> loop :: Eval [S]
> loop = sequence (repeat step)

> step :: Eval S
> step = do
>   op <- gets (curr_ . prog'_) -- fetch the current OP to execute
>   exec op                     -- execute it
>   get                         -- return the current state

Executing an operation.

The "microcode" `v` extracts the value out of a literal:

  - if it's a label, get the current value of the named register (0 if unset)
  - the number itself otherwise

> v :: Lit -> Eval Int
> v (L x) = (fromMaybe 0 . M.lookup x) <$> gets regs_
> v (N x) = pure x

Effectful execution of 1 OP.

> exec :: OP -> Eval ()
> exec (SND l1      ) = do { _snd =<< v l1        ; _jump 1 }
> exec (SET (L x) l2) = do { _set x =<< v l2      ; _jump 1 }
> exec (ADD (L x) l2) = do { _op2 (+)   x =<< v l2; _jump 1 }
> exec (MUL (L x) l2) = do { _op2 (*)   x =<< v l2; _jump 1 }
> exec (MOD (L x) l2) = do { _op2 (mod) x =<< v l2; _jump 1 }
> exec (RCV (L x)   ) = do { _rcv x               ; _jump 1 }
> exec (JGZ (L x) l2) = do { _jgz x =<< v l2                }

The respective implementations which modify the current state of the machine.

> _jump :: Int -> Eval ()
> _jump offset = modify (\s -> s { prog'_ = jump offset (prog'_ s) } )

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
>   x <- M.findWithDefault 0 reg <$> gets regs_
>   case x of
>     0 -> _jump 1
>     n -> _jump offset

Problem's solution: parse the program and execute it until a frequency is
recovered.

> main :: IO ()
> main = do
>   Right program <- parse parseProgram "input.txt" <$> readFile "input.txt"
>   print . head . dropWhile (null . recovered_) . trace $ program
