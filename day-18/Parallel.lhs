> {-# LANGUAGE TupleSections #-}
> module Main where

> import Control.Monad
> import Text.Parsec        hiding (State)
> import Text.Parsec.String

> import Data.Maybe
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict as M
> import Control.Monad.Trans.State.Lazy

> import Data.Sequence (Seq, ViewL(..), (|>))
> import qualified Data.Sequence as S

> import System.Environment

> import Debug.Trace hiding (trace)
> import Data.List (intercalate)

> data Lit
>   = L Char           -- Labels: 'a', 'b', ...
>   | N Int            -- Numbers: 3, -9, ...
>   deriving (Show)

<<<<<Explain what the OP things are, orig and new for the simplifier>>>>>

> data OP
>   = SND Lit          -- snd X    :  play frequency X
>   | SET Lit Lit      -- set X Y  :  X = Y
>   | ADD Lit Lit      -- add X Y  :  X += Y
>   | MUL Lit Lit      -- mul X Y  :  X *= Y
>   | MOD Lit Lit      -- mod X Y  :  X %= Y
>   | RCV Lit          -- rcv X    :  retrieve last frequency played
>   | JGZ Lit Lit      -- jgz X Y  :  if (X>0) jump by Y
>   -- the simplifier maps JGZ (N _) to either of these:
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
>       "mul" -> MUL <$> lit <*> lit
>       "mod" -> MOD <$> lit <*> lit
>       "rcv" -> RCV <$> lit
>       "jgz" -> JGZ <$> lit <*> lit
>       _ -> error "unimplemented op found in program"

Let's simplify the program if possible. That Wastl dude has

> simplify :: Program -> Program
> simplify = map simplifyJumps
>   where
>     simplifyJumps (JGZ (N n) l)
>       | n > 0     = JMP l
>       | otherwise = NOP
>     simplifyJumps x = x

A program "zipper" that focuses on an instruction. Will need this for jumping.

> data Program' = P' { left_ :: [OP], curr_ :: OP, right_ :: [OP] }
>   deriving (Show)

> zipProgram :: Program -> Program'
> zipProgram (o:os) = P' [] o os
> zipProgram _ = error "empty program"

> jump :: Int -> Program' -> Maybe Program'
> jump 0 p' = Just p'
> jump 1    (P' _  _ []    ) = Nothing
> jump 1    (P' ls x (r:rs)) = Just (P' (x:ls) r rs)
> jump (-1) (P' []     _ _ ) = Nothing
> jump (-1) (P' (l:ls) x rs) = Just (P' ls l (x:rs))
> jump n p' | n > 0 = jump   1  p' >>= jump (n-1)
> jump n p' | n < 0 = jump (-1) p' >>= jump (n+1)

A program can now be running or paused.

> data Status
>   = Running  -- 🏃
>   | Paused   -- Waiting on an `RCV` instruction
>   | Ended    -- No more instructions to execute
>   deriving (Eq, Show)

The state for running a single program.

> data S = S
>   { prog'_  :: Program'
>   , regs_   :: Map Char Int
>   , queue_  :: Seq Int
>   , status_ :: Status
>   } deriving (Show)

> fromProgram :: Int -> Program -> S
> fromProgram n p = S
>   { prog'_  = zipProgram p
>   , regs_   = M.singleton 'p' n
>   , queue_  = S.fromList []
>   , status_ = Running
>   }

The machine state `S2` for parallel program evaluation.
`count_` is going to be used to answer the "problem 2" question.
I.e. to keep track of the number of times program 1 sends a value.

> data S2 = S2 { p0_ :: S , p1_ :: S, count_ :: Int } deriving (Show)

Generate a machine state from a program to be executed in parallel with itself.

> fromProgram2 :: Program -> S2
> fromProgram2 p = S2
>   { p0_ = fromProgram 0 p
>   , p1_ = fromProgram 1 p
>   , count_ = 0
>   }

Helper function to poke at one program or the other.

> p_ :: Int -> S2 -> S
> p_ 0 = p0_
> p_ 1 = p1_
> p_ _ = error "this parallel machine supports only 2 programs"

> modifyN :: Int -> (S -> S) -> Eval ()
> modifyN 0 f = modify $ \s2 -> s2 { p0_ = f (p0_ s2) }
> modifyN 1 f = modify $ \s2 -> s2 { p1_ = f (p1_ s2) }
> modifyN _ _ = error "this parallel machine supports 2 programs"

Lazy state monad to capture the computation of an S2 machine.

> type Eval = State S2

Tracing the execution of two parallel programs.

> trace2 :: Program -> Int
> trace2 = evalState loop2 . fromProgram2

> loop2 :: Eval Int
> loop2 = loop2' 0 0 0
>   where
>     loop2' k i0 i1 = do
>       iter1 <- runUntilPaused 1
>       iter0 <- runUntilPaused 0
>       s0 <- gets (status_ . p0_)
>       s1 <- gets (status_ . p1_)
>       p0 <- gets (p0_)
>       p1 <- gets (p1_)
>       s2 <- get
>       let i0' = i0 + iter0
>       let i1' = i1 + iter1
>       when (k `mod` 1000 == 0) $ do
>         traceM . intercalate "\t" . map show $ [k,i0',i1']
>       case (s0, S.null $ queue_ p0, s1, S.null $ queue_ p1) of
>         (Paused, True, Paused, True) -> gets count_  -- Deadlock
>         _ -> loop2' (k+1) i0' i1'

       -- traceM (show s2)
       -- traceM . intercalate "\t" $
       --   ["0:",show iter0,show (queue_ p0),show (curr_ $ prog'_ p0)]
       -- traceM . intercalate "\t" $
       --   ["1:",show iter1,show (queue_ p1),show (curr_ $ prog'_ p1)]

> runUntilPaused :: Int -> Eval Int
> runUntilPaused n = go 1
>   where
>     go i = do
>       step n
>       s <- gets (status_ . p_ n)
>       case s of
>         Paused -> return i
>         _      -> go (i+1)

Execute one step of the n-th program (n=0,1)

> step :: Int -> Eval ()
> step n = do
>   op <- gets (curr_ . prog'_ . p_ n)
>   newStatus <- exec n op
>   -- traceShow (n,op,newStatus) (return ())
>   c <- gets count_
>   seq c (return ())
>   -- traceShow (n,c) (return ())
>   modifyN n (\s -> s { status_ = newStatus })

Counting for the problem's solution.

> incrementProblemCount :: Eval ()
> incrementProblemCount = do
>   -- traceM "1 incremented the count"
>   modify (\s2 -> s2 { count_ = succ (count_ s2) })

> v :: Int -> Lit -> Eval Int
> v n (L x) = (fromMaybe 0 . M.lookup x) <$> gets (regs_ . p_ n)
> v _ (N x) = pure x

Effectful execution of 1 OP.

> exec :: Int -> OP -> Eval Status
> exec n (SND l1      ) = do { _snd n =<< v n l1         ; _jump n 1 }
> exec n (SET (L x) l2) = do { _set n x =<< v n l2       ; _jump n 1 }
> exec n (ADD (L x) l2) = do { _op2 n (+)   x =<< v n l2 ; _jump n 1 }
> exec n (MUL (L x) l2) = do { _op2 n (*)   x =<< v n l2 ; _jump n 1 }
> exec n (MOD (L x) l2) = do { _op2 n (mod) x =<< v n l2 ; _jump n 1 }
> exec n (RCV (L x)   ) = do { _rcv n x                              }
> exec n (JGZ (L x) l2) = do { _jgz n x =<< v n l2                   }
> exec n (JMP l1      ) = do { _jump n =<< v n l1                    }
> exec n (NOP         ) = do { _jump n 1                             }

The respective implementations which modify the current state of the machine.

> _jump :: Int -> Int -> Eval Status
> _jump n offset = do
>   hasJumped <- jump offset <$> gets (prog'_ . p_ n)
>   case hasJumped of
>     Just p' -> do modifyN n (\s -> s { prog'_ = p' })
>                   return Running
>     Nothing -> return Ended

> _set :: Int -> Char -> Int -> Eval ()
> _set n reg val = modifyN n
>   (\s -> s { regs_ = M.alter (Just . const val) reg $ regs_ s })

> _op2 :: Int -> (Int -> Int -> Int) -> Char -> Int -> Eval ()
> _op2 n (@@) reg val = modifyN n
>   (\s -> s { regs_ = M.adjust (@@ val) reg $ regs_ s })

> _snd :: Int -> Int -> Eval ()
> _snd n val = do
>   let m = (succ n `mod` 2)
>   when (n == 1) incrementProblemCount
>   modifyN m (\s -> s { queue_ = (queue_ s) |> val })

> _rcv :: Int -> Char -> Eval Status
> _rcv n reg = do
>   q <- gets (S.viewl . queue_ . p_ n)
>   case q of
>     EmptyL -> return Paused
>     (x :< xs) -> do
>       modifyN n (\s -> s { queue_ = xs })
>       _set n reg x
>       _jump n 1

> _jgz :: Int -> Char -> Int -> Eval Status
> _jgz n reg offset = do
>   x <- M.findWithDefault 0 reg <$> gets (regs_ . p_ n)
>   if x > 0
>   then _jump n offset
>   else _jump n 1

> main :: IO ()
> main = do
>   [file] <- getArgs
>   Right program <- parse parseProgram file <$> readFile file
>   print . execState loop2 . fromProgram2 . simplify $ program
