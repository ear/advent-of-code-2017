> {-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
> import Data.Ord
> import Data.List
> import Data.Function
> import Control.Monad
> import Control.Arrow
> import Control.Exception
> import Data.Vector.Unboxed (Vector)
> import qualified Data.Vector.Unboxed as V
> import qualified Data.Vector.Unboxed.Mutable as MV
> import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------

Can I calculate how big the image will be after n "enhancing" steps?

step size                  class of rule used
0    3
1    (3/3)*4 = 4           3
2    (4/2)*3 = 2*3 = 6     2
3    (6/2)*3 = 3*3 = 9     2
4    (9/3)*4 = 3*4 = 12    3
5    (12/2)*3 = 6*3 = 18   2
6    (18/2)*3 = 9*3 = 27   2
7    (27/3)*4 = 9*4 = 36   3
8    (36/2)*3 = 18*3 = 54  2
9    (54/2)*3 = 27*3 = 81  2
10   (81/3)*4 = 27*4 = ..  3

> sizes :: [Int]
> sizes = 3 : 4 : 6 : map (3*) sizes

> size :: Int -> Int
> size = (sizes !!)

Yep.

--------------------------------------------------------------------------------

Let's look at the change in number of pixels.

> parse :: String -> [[Int]]
> parse = map (map (\case '#' -> 1; '.' -> 0))
>       . lines . filter (`notElem` "=> /")

> count :: [Int] -> (Int,Int)
> count = (sum *** sum) . uncurry splitAt . (l &&& id)
>   where l xs | length xs == 2*2 + 3*3 = 2*2
>              | length xs == 3*3 + 4*4 = 3*3

> test_counts :: IO ()
> test_counts = do
>   cs <- map count . parse <$> readFile "input.txt"
>   let sort' = sortBy (comparing fst)
>       (cs2,cs3) = (sort' *** sort') . splitAt 6 $ cs
>       table = map (first head . unzip) . groupBy ((==) `on` fst)
>   putStrLn "pixel transition counts for size 2:"
>   mapM_ print . table $ cs2
>   putStrLn "pixel transition counts for size 3:"
>   mapM_ print . table $ cs3

This table shows, for each group of rules, for each amount of bits in the input,
the amount of bits that can be produced by a rule from the `input.txt`.

    pixel transition counts for size 2:
    (0,[6])
    (1,[4])
    (2,[7,5])
    (3,[7])
    (4,[7])
    pixel transition counts for size 3:
    (0,[9])
    (1,[10,8,6])
    (2,[7,8,11,10,8,6,8,11])
    (3,[10,5,9,7,9,7,10,13,6,10,6,11,5,9,6,7])
    (4,[11,9,8,9,8,9,7,8,7,7,12,7,10,7,10,6,10,7,8,4,10,9,6])
    (5,[6,7,6,10,6,7,8,4,8,7,9,6,7,5,9,6,8,11,8,9,9,7,9])
    (6,[6,8,6,9,8,8,7,10,5,8,8,7,5,6,5,6])
    (7,[4,8,6,9,10,10,8,10])
    (8,[7,9,7])
    (9,[10])

It is not possible to know the number of bits in the output solely by counting
the number of bits in the input.

Thus one can not calculate the number of bits of the n-th output without
performing the whole "enhancing" process of matching and replacing iteratively
according to the rules.

--------------------------------------------------------------------------------

I can simplify the process by creating a datastructure that contains entries for
all the possible symmetric matches of each rule's input.

--------------------------------------------------------------------------------

The next question is what are the required symmetries.

The group of symmetries of a square is D8, the dihedral group of order 8.

I am going to represent both the symmetries and the squares as unboxed vectors.

> newtype Sym = Sym (V.Vector Int) deriving (Show, Eq)
> mkS = Sym . V.fromList  -- this will be fed lists of size (2*2) or (3*3)

> (•) :: Sym -> Sym -> Sym
> (Sym s) • (Sym p) = Sym $ V.backpermute s p
> infixl 5 •

> class Symmetric t where
>   (@@) :: t -> Sym -> t  -- applies a symmetry

Squares of size 2.

> newtype Sq2 = Sq2 (V.Vector Int) deriving (Eq)
> mkSq2 = Sq2 . V.fromListN (2*2)  -- here we can let the compiler know the size

> instance Show Sq2 where
>   show (Sq2 v) = intercalate "\n" $ map (concat . map show) $ [[a,b],[c,d]]
>     where [a,b,c,d] = V.toList v

> sq2 = mkSq2 [0,1,2,3]

Size 2 symmetries.

> instance Symmetric Sq2 where
>   (Sq2 v) @@ (Sym sym) = Sq2 $ V.backpermute v sym

> s2syms = r2s ++ f2s
> r2s = take 4 . iterate (• r2) $ r2 where r2 = mkS [2,0,3,1]  -- rotations
> f2s = [mkS [1,0,3,2], mkS [2,3,0,1]   -- flips along the axes
>       ,mkS [0,2,1,3], mkS [3,1,2,0]]  -- flips along the diagonals

Size 3.

> newtype Sq3 = Sq3 (V.Vector Int)
> mkSq3 = Sq3 . V.fromListN (3*3)

> instance Show Sq3 where
>   show (Sq3 v) = intercalate "\n" . map (concat . map show) $ square
>     where
>       [a,b,c,d,e,f,g,h,i] = V.toList v
>       square = [[a,b,c],[d,e,f],[g,h,i]]

> sq3 = mkSq3 [0,1,2,3,4,5,6,7,8]

Size 3 symmetries.

There are more degrees of freedom here. Some symmetries are not simple rotations
or flips about an axis but their composition.

> instance Symmetric Sq3 where
>   (Sq3 v) @@ (Sym sym) = Sq3 $ V.backpermute v sym

> s3syms = generateS $ r3s ++ f3s
> r3s = take 8 . iterate (• r3) $ r3 where r3 = mkS [3,0,1, 6,4,2, 7,8,5]
> f3s = [mkS [2,1,0, 5,4,3, 8,7,6], mkS [6,7,8, 3,4,5, 0,1,2]   -- axes
>       ,mkS [0,3,6, 1,4,7, 2,5,8], mkS [8,5,2, 7,4,1, 6,3,0]]  -- diagonals

> generateS = nub . map (uncurry (•)) . join (liftM2 (,))

This tests the group generated by those symmetries is exactly itself.

> test_syms = all test [s2syms,s3syms]
>   where test ss = ((==) `on` sortedVectors) ss (generateS ss)
>         sortedVectors = nub . sort . map (\(Sym s) -> s)

--------------------------------------------------------------------------------

Armed with the symmetries we can go on to solve the problem.

(1) make a Map

Let's deal in just `Square`s. Where `size_` is their side.

> data Square = S { size_ :: Int, v_ :: V.Vector Int } deriving (Show, Ord, Eq)

> toSquare :: [Int] -> Square
> toSquare = uncurry S . (sqrt' . fst &&& uncurry V.fromListN) . (length &&& id)
>   where sqrt' = truncate . sqrt . fromIntegral

> class Squared t where
>   fromSq :: t -> Square
> instance Squared Sq2 where
>   fromSq (Sq2 v) = S 2 v
> instance Squared Sq3 where
>   fromSq (Sq3 v) = S 3 v

> instance Symmetric Square where
>   (S n v) @@ (Sym m) = assert (n*n == V.length m) (S n $ V.backpermute v m)

> syms :: Int -> [Sym]
> syms 2 = s2syms
> syms 3 = s3syms
> syms _ = error "don't have symmetries for this size square"

The book of enhancements. It is augmented with copies of each rule that match
symmetric inputs.

> data Enhancements = E { map_ :: M.Map Square Square } deriving (Show)

> parseEnhancements :: String -> Enhancements
> parseEnhancements = foldl' collect (E M.empty) . parse

> collect :: Enhancements -> [Int] -> Enhancements
> collect (E m) inout = let
>
>   -- quirk from dirty parsing
>   n = case length inout of { 13 -> 2; 25 -> 3 }
>   (is,os) = splitAt (n*n) inout
>
>   -- (i)nput square, (o)utput square
>   i = S { size_ = n    , v_ = V.fromListN (n*n)         is }
>   o = S { size_ = (n+1), v_ = V.fromListN ((n+1)*(n+1)) os }
>
>   -- all conceivable symmetric inputs for this size square
>   ii = (i @@) <$> syms n
>
>   -- paired with the same output
>   m' = M.fromList $ map (,o) ii
>
>   -- added to the tally of enhancement rules
>   in E $ M.union m' m

(2) iterate the process

> start = S { size_ = 3, v_ = V.fromList [0,1,0, 0,0,1, 1,1,1] }

> enhance :: Enhancements -> Square -> Maybe Square
> enhance (map_ -> m) s = let
>   -- size of the square
>   l = size_ s
>
>   -- size of the sub-squares
>   sl = case l of
>          _ | l `mod` 2 == 0 -> 2
>          _ | l `mod` 3 == 0 -> 3
>
>   -- number of sub-squares
>   sn = (l `div` sl)^2
>
>   -- sub-squares
>   ss = chop s sn sl
>
>   -- map sub-squares to their respective enhancements (in the maybe monad)
>   es = sequence $ (`M.lookup` m) <$> ss
>
>   -- put together the enhanced image
>   in unchop <$> es

> chop :: Square -> Int -> Int -> [Square]
> chop s         1 _  = [s]
> chop s@(S l v) n sl = let
>   n' = round . sqrt . fromIntegral $ n
>   extract k = V.create $ do
>     mv <- V.thaw v
>     smv <- MV.new (sl*sl)
>     forM [0 .. sl-1] $ \j -> do
>       let slice = MV.slice (k+j*l) sl mv
>       writeSlice smv slice (j*sl) sl
>     return smv
>   in S sl <$> map extract [l*sl*k + sl*i | k <- [0 .. n'-1], i <- [0 .. n'-1]]

> writeSlice mdest msrc i len = do
>   forM [0 .. len-1] $ \j -> do
>     x <- MV.read msrc j
>     MV.write mdest (i+j) x

> unchop :: [Square] -> Square
> unchop ss = S l v
>   where
>     n = length ss
>     n' = round . sqrt . fromIntegral $ n
>     sl = size_ (head ss)
>     l = n'*sl
>     ss' = map v_ ss
>     v = V.create $ do
>       mv <- MV.new (l*l)
>       let idx = zip [0..] [l*sl*k + sl*i | k <- [0 .. n'-1], i <- [0 .. n'-1]]
>       forM idx $ \(si,k) -> do
>         ms <- V.thaw (ss' !! si)
>         forM [0 .. sl-1] $ \j -> do
>           let slice = MV.slice (j*sl) sl ms
>           writeSlice mv slice (k+j*l) sl
>       return mv

> enhances es = unfoldr (((id &&& id) <$>) . enhance es)

Chop is finally good. Unchop is WIP. :D

(3) count the bits

~
