> {-# LANGUAGE TupleSections #-}
> module Main where

> import Control.Monad
> import Text.Parsec        hiding (State)
> import Text.Parsec.String

> import Data.Maybe
> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict as M
> import Control.Monad.Trans.State.Lazy

> import System.Environment

> import Debug.Trace hiding (trace)
> import Data.List

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
>       _ -> error "unimplemented op found in program"

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
>   , queue_  :: [Int]
>   , status_ :: Status
>   } deriving (Show)

> fromProgram :: Int -> Program -> S
> fromProgram n p = S
>   { prog'_  = zipProgram p
>   , regs_   = M.singleton 'p' n
>   , queue_  = []
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

> getsN :: Int -> (S -> a) -> Eval a
> getsN n f = gets (f . p_ n)

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
> loop2 = do
>   step 0
>   step 1
>   s0 <- gets (status_ . p_ 0)
>   q0 <- gets (queue_ . p_ 0)
>   op0 <- gets (curr_ . prog'_ . p_ 0)
>   s1 <- gets (status_ . p_ 1)
>   q1 <- gets (queue_ . p_ 1)
>   op1 <- gets (curr_ . prog'_ . p_ 1)
>   when (s0 == Paused) $ traceM . show $ (op0,s0,q0,op1,s1,q1)
>   case (s0,s1) of
>     (Paused,Paused) -> gets count_
>     _ -> loop2

loop2 :: Eval Int
loop2 = do
  iter0 <- runUntilPaused 0
  iter1 <- runUntilPaused 1
  s0 <- gets (status_ . p0_)
  s1 <- gets (status_ . p1_)
  p0 <- gets (p0_)
  p1 <- gets (p1_)
  s2 <- get
  when (count_ s2 `mod` 1 == 0) $ traceM (show s2)
  -- traceM . intercalate "\t" $
  --   ["0:",show iter0,show (queue_ p0),show (curr_ $ prog'_ p0)]
  -- traceM . intercalate "\t" $
  --   ["1:",show iter1,show (queue_ p1),show (curr_ $ prog'_ p1)]
  case (s0, queue_ p0, s1, queue_ p1) of
    (Paused, [], Paused, []) -> gets count_  -- Deadlock
    _ -> loop2

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
> exec n (SND l1      ) = do { _snd n =<< v n l1        ; _jump n 1 }
> exec n (SET (L x) l2) = do { _set n x =<< v n l2      ; _jump n 1 }
> exec n (ADD (L x) l2) = do { _op2 n (+)   x =<< v n l2; _jump n 1 }
> exec n (MUL (L x) l2) = do { _op2 n (*)   x =<< v n l2; _jump n 1 }
> exec n (MOD (L x) l2) = do { _op2 n (mod) x =<< v n l2; _jump n 1 }
> exec n (RCV (L x)   ) = do { _rcv n x                             }
> exec n (JGZ (L x) l2) = do { _jgz n x =<< v n l2                  }

The respective implementations which modify the current state of the machine.

> _jump :: Int -> Int -> Eval Status
> _jump n offset = do
>   x <- jump offset <$> getsN n prog'_
>   case x of
>     Just p' -> do { modifyN n (\s -> s { prog'_ = p' } ); return Running }
>     Nothing -> return Ended

> _snd :: Int -> Int -> Eval ()
> _snd n val = let n' = succ n `mod` 2 in do -- id of the other process
>   modifyN n' (\s -> s { queue_ = val : queue_ s })
>   if (n == 1) then incrementProblemCount else return ()
>   -- traceShow (n,"send",val) $ return ()

> _set :: Int -> Char -> Int -> Eval ()
> _set n reg val = modifyN n (\s ->
>   s { regs_ = M.alter (Just . const val) reg $ regs_ s })

> _op2 :: Int -> (Int -> Int -> Int) -> Char -> Int -> Eval ()
> _op2 n (@@) reg val = modifyN n (\s ->
>   s { regs_ = M.adjust (@@ val) reg $ regs_ s })

> _rcv :: Int -> Char -> Eval Status
> _rcv n reg = do
>   -- traceShow (n,"receive on",reg) $ return ()
>   q <- gets (queue_ . p_ n)
>   case q of
>     [] -> return Paused -- empty queue, pause.
>     (x:rest) -> do
>       modifyN n (\s -> s { queue_ = rest })
>       _set n reg x
>       _jump n 1

> _jgz :: Int -> Char -> Int -> Eval Status
> _jgz n reg offset = do
>   x <- M.findWithDefault 0 reg <$> gets (regs_ . p_ n)
>   case x of
>     0 -> _jump n 1
>     _ -> _jump n offset


SO I think I am deadlocking haha that is kind of ironic!!

Should check if
 - programs get paused
 - programs get UN-paused
Via showing SND and RCV contextually with the pid and the pstatus.

> main :: IO ()
> main = do
>   [fileName] <- getArgs
>   Right program <- parse parseProgram fileName <$> readFile fileName
>   print . execState loop2 . fromProgram2 $ program