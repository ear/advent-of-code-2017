> module Main where

> import Control.Monad
> import Text.Parsec        hiding (State)
> import Text.Parsec.String

> import Data.Maybe
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict as M
> import Control.Monad.Trans.State.Lazy

> import Debug.Trace hiding (trace)

> data Lit
>   = L Char           -- Labels: 'a', 'b', ...
>   | N Int            -- Numbers: 3, -9, ...
>   deriving (Show)

> data OP
>   = SND Lit          -- snd X    :  play frequency X
>   | SET Lit Lit      -- set X Y  :  X = Y
>   | ADD Lit Lit      -- add X Y  :  X += Y
>   | SUB Lit Lit      -- sub X Y  :  X -= Y
>   | MUL Lit Lit      -- mul X Y  :  X *= Y
>   | MOD Lit Lit      -- mod X Y  :  X %= Y
>   | RCV Lit          -- rcv X    :  retrieve last frequency played
>   | JGZ Lit Lit      -- jgz X Y  :  if (X>0) jump by Y
>   | JNZ Lit Lit      -- jnz X Y  :  if (X≠0) jump by Y
>   -- the simplifier maps J?Z (N _) to either of these:
>   | JMP Lit          -- jmp X    :  uncoditional jump
>   | NOP              -- nop      :  no operation (keeps other jumps correct)
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
>       "sub" -> SUB <$> lit <*> lit
>       "mul" -> MUL <$> lit <*> lit
>       "mod" -> MOD <$> lit <*> lit
>       "rcv" -> RCV <$> lit
>       "jgz" -> JGZ <$> lit <*> lit
>       "jnz" -> JNZ <$> lit <*> lit
>       _ -> error "parser found an unrecognizable instruction"

> simplify :: Program -> Program
> simplify = map simplifyJumps
>   where
>     simplifyJumps (JGZ (N n) l)
>       | n > 0     = JMP l
>       | otherwise = NOP
>     simplifyJumps (JNZ (N n) l)
>       | n /= 0    = JMP l
>       | otherwise = NOP
>     simplifyJumps x = x

A program "zipper" that focuses on an instruction. Will need this for jumping.

> data Program' = P' { left_ :: [OP], curr_ :: OP, right_ :: [OP] }
>   deriving (Show)

> zipProgram :: Program -> Program'
> zipProgram (o:os) = P' [] o os
> zipProgram _ = error "empty program"

> jump :: Int -> Program' -> Maybe Program'
> jump 0    p'                   = Just p'
> jump 1    (P' _      _ []    ) = Nothing
> jump 1    (P' ls     x (r:rs)) = Just (P' (x:ls) r rs)
> jump (-1) (P' []     _ _     ) = Nothing
> jump (-1) (P' (l:ls) x rs    ) = Just (P' ls l (x:rs))
> jump n p' | n > 0 = jump 1    p' >>= jump (n-1)
> jump n p' | n < 0 = jump (-1) p' >>= jump (n+1)

The machine state `S` for the program evaluation.

> data S = S
>   { count_ :: Int         -- Instruction count
>   , prog'_ :: Program'    -- Zipper to keep track of the program execution
>   , regs_ :: Map Char Int -- Map of register names to register values
>   , played_ :: [Int]      -- (Reversed) history of played frequencies
>   , recovered_ :: [Int]   -- (Reversed) history of recovered frequencies
>   , day23_ :: Int         -- Part 1: the number of mul instructions performed
>   } deriving (Show)

> fromProgram :: Program -> S
> fromProgram p = S
>   { count_ = 0
>   , prog'_ = zipProgram p
>   , regs_ = M.fromList $ zip ['a'..'h'] (repeat 0)
>   , played_ = []
>   , recovered_ = []
>   , day23_ = 0 }

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
>   modify (\s -> s { count_ = succ (count_ s) })
>   k <- gets count_
>   -- when (k `mod` 1000000 == 0) $ do
>   traceM (show op)
>   traceM . show =<< get
>   -- when (k > 200) $ fail "done"
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
> exec (SND l1      ) = do { _snd =<< v l1             ; _jump 1         }
> exec (SET (L x) l2) = do { _set x =<< v l2           ; _jump 1         }
> exec (ADD (L x) l2) = do { _op2 "+" (+)        x =<< v l2; _jump 1         }
> exec (SUB (L x) l2) = do { _op2 "-" (subtract) x =<< v l2; _jump 1         }
> exec (MUL (L x) l2) = do { _day23; _op2 "*" (*)        x =<< v l2; _jump 1 }
> exec (MOD (L x) l2) = do { _op2 "mod" (mod)      x =<< v l2; _jump 1         }
> exec (RCV (L x)   ) = do { _rcv x                    ; _jump 1         }
> exec (JGZ (L x) l2) = do {                             _jgz x =<< v l2 }
> exec (JNZ (L x) l2) = do {                             _jgz x =<< v l2 }
> exec (JMP l1      ) = do {                             _jump =<< v l1  }
> exec (NOP         ) = do {                             _jump 1         }

The respective implementations which modify the current state of the machine.

> _day23 :: Eval ()
> _day23 = traceId "mul!" `seq` modify (\s -> s { day23_ = succ (day23_ s) })

> _jump :: Int -> Eval ()
> _jump offset = do
>   p' <- gets prog'_
>   case jump offset p' of
>     Nothing -> fail "program ended"
>     Just p'' -> modify (\s -> s { prog'_ = p'' })

> _snd :: Int -> Eval ()
> _snd freq = modify (\s -> s { played_ = freq : played_ s })

> _set :: Char -> Int -> Eval ()
> _set reg val = modify (\s ->
>   s { regs_ = M.alter (Just . const val) reg $ regs_ s })

> _op2 :: String -> (Int -> Int -> Int) -> Char -> Int -> Eval ()
> _op2 op (@@) reg val = do
>   traceM . concat $ [show reg, "=", show reg, " ", op, " ", show val]
>   modify (\s -> s { regs_ = M.adjust (@@ val) reg $ regs_ s })

_op2 :: (Int -> Int -> Int) -> Char -> Int -> Eval ()
_op2 (@@) reg val = modify (\s ->
  s { regs_ = M.adjust (@@ val) reg $ regs_ s })

> _rcv :: Char -> Eval ()
> _rcv reg = do
>   Just x <- M.lookup reg <$> gets regs_
>   when (x > 0) $ modify (\s ->
>     s { recovered_ = head (played_ s) : (recovered_ s) })

> _jgz :: Char -> Int -> Eval ()
> _jgz reg offset = do
>   x <- M.findWithDefault 0 reg <$> gets regs_
>   if x > 0
>   then _jump offset
>   else _jump 1

> _jnz :: Char -> Int -> Eval ()
> _jnz reg offset = do
>   x <- M.findWithDefault 0 reg <$> gets regs_
>   case x of
>     0 -> _jump 1
>     _ -> _jump offset

Problem's solution: parse the program and execute it until a frequency is
recovered.

> main :: IO ()
> main = do
>   Right program <- parse parseProgram "simplified.txt" <$> readFile "simplified.txt"
>   -- mapM_ print $ simplify program
>   mapM_ print . trace . simplify $ program
