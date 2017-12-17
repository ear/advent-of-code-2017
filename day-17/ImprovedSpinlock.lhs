> {-# LANGUAGE PartialTypeSignatures #-}
> module Main where

So, as it turns out, I *am* going to need these.

> import Prelude hiding (max)

> import Control.Monad
> import Control.Monad.ST

> import Data.Word
> import Data.STRef

> import Data.Vector.Unboxed ((!))
> import qualified Data.Vector.Unboxed as V
> import qualified Data.Vector.Generic.Mutable as M

> import Debug.Trace

For a max of 50˙000˙000 the type `Word32` is not enough:

    > maxBound :: Data.Word.Word32
    4294967295
    > maxBound :: Data.Word.Word64
    18446744073709551615
    > maxBound :: Int
    9223372036854775807

`Int` would also work.

I am also going to keep track of the position of `0` to avoid having to search
it through all the memory at the end.

> data Mem = M
>   { mem_  :: !(V.Vector Word64)
>   , size_ :: !Int
>   , pos_  :: !Int
>   , zeroPos_ :: !Int
>   } deriving Show

The total amount of memory is the number of numbers to insert, plus 1 for the
initial 0.

> limit :: Int
> limit = 1 + 2017
> -- limit = 1 + 50000000

In the 50 million case this big of an empty memory needs almost 1GB of RAM! ha.

> empty :: Mem
> empty = M
>   { mem_     = V.fromList $ replicate limit 0
>   , size_    = 1
>   , pos_     = 0
>   , zeroPos_ = 0 }

> step :: Int
> step = 359

> spinlock :: Mem -> Int -> Mem
> spinlock (M m msize mpos mzeroPos) max = runST $ do
>   v <- V.thaw m
>   _size    <- newSTRef msize
>   _pos     <- newSTRef mpos
>   _zeroPos <- newSTRef mzeroPos
>   forM_ [1 .. fromIntegral max] $ \x -> do    -- insert each of [1, ..., max]
>     -- when (x `mod` 10000 == 0) $ traceShow x (return ())
>     size <- readSTRef _size
>     pos  <- readSTRef _pos
>     let insertPos = succ $ (pos + step) `mod` size
>     writeSTRef _pos insertPos
>     unless (insertPos == size) $ do           -- if not at the end, shift by 1
>       _x <- newSTRef =<< M.read v insertPos
>       forM_ [insertPos .. size-1] $ \i -> do
>         x' <- readSTRef _x
>         y <- M.exchange v (i+1) x'
>         writeSTRef _x y
>         return ()
>     M.write v insertPos x
>     writeSTRef _size (succ size)
>     writeSTRef _pos insertPos
>   m' <- V.freeze v
>   size'    <- readSTRef _size
>   pos'     <- readSTRef _pos
>   zeroPos' <- readSTRef _zeroPos
>   return (M m' size' pos' zeroPos')

And now print the value after 2017.

> main :: IO ()
> main = let
>   m = spinlock empty (limit-1)
>   Just i = V.findIndex (2017==) $ mem_ m
>   after2017 = mem_ m ! (succ i `mod` limit)
>   in print after2017

This is the same thing, but for part 2. Search for 0, show the element after it.

main :: IO ()
main = let
  m = spinlock empty (limit-1)
  Just i = V.findIndex (0==) $ mem_ m
  after0 = mem_ m ! succ i0
  in print after0

Unfortunately it takes forever. Being 500x faster than `Spinlock.lhs` is not
nearly enough! A different approach is needed.
