> {-# LANGUAGE LambdaCase, ViewPatterns #-}
> import Data.List
> import Control.Monad
> import Control.Arrow hiding (left, right)
> import Control.Monad.Trans.State.Lazy

> main :: IO ()
> main = do
>   print . fst . runState (replicateM 12173597 step >> diagnostic) $ begin A

> data States = A | B | C | D | E | F deriving (Show, Eq)

> data Tape = T { l_ :: [Int], x_ :: Int, r_ :: [Int] } deriving (Show, Eq)

> tl (T []     x rs) = T []     0 (x:rs)
> tl (T (l:ls) x rs) = T ls     l (x:rs)
> tr (T ls x []    ) = T (x:ls) 0 []
> tr (T ls x (r:rs)) = T (x:ls) r rs

> t0 = T [] 0 []

> tdiagnostic (T ls x rs) = sum ls + x + sum rs

> data Machine = M { k_ :: Int, s_ :: States, t_ :: Tape } deriving (Show, Eq)

> begin s = M 0 s t0

> type Turing = State Machine

> readL = gets s_
> writeL v = modify $ \s -> s { s_ = v }

> readV = gets $ x_ . t_
> writeV n = modify $ \s -> s { t_ = (\tape -> tape { x_ = n }) (t_ s) }

> change :: States -> Turing ()
> change letter = modify $ \s -> s { s_ = letter }

> left, right :: Turing ()
> left = modify $ \s -> s { t_ = tl (t_ s) }
> right = modify $ \s -> s { t_ = tr (t_ s) }

> step = do
>   l <- readL
>   v <- readV
>   case l of
>     A -> case v of
>       0 -> writeV 1 >> right >> writeL B
>       1 -> writeV 0 >> left  >> writeL C
>     B -> case v of
>       0 -> writeV 1 >> left  >> writeL A
>       1 -> writeV 1 >> right >> writeL D
>     C -> case v of
>       0 -> writeV 1 >> right >> writeL A
>       1 -> writeV 0 >> left  >> writeL E
>     D -> case v of
>       0 -> writeV 1 >> right >> writeL A
>       1 -> writeV 0 >> right >> writeL B
>     E -> case v of
>       0 -> writeV 1 >> left  >> writeL F
>       1 -> writeV 1 >> left  >> writeL C
>     F -> case v of
>       0 -> writeV 1 >> right >> writeL D
>       1 -> writeV 1 >> right >> writeL A

> diagnostic = gets (tdiagnostic . t_)
