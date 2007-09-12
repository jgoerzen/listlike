{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

module TestInfrastructure where

import Test.QuickCheck
import Test.QuickCheck.Batch
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ListLike as LL
import qualified Data.Map as Map
import qualified Data.Array as A
import qualified Data.Foldable as F
import System.Random
import Test.HUnit
import Text.Printf
import Data.Word
import Data.List
import Data.Monoid

instance (LL.ListLike f i, Arbitrary i) => Arbitrary f where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- ((vector n)::Gen [i])
                     return (LL.fromList (arblist::[i]))
    {-
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- sequence [ arbitrary | i <- [1..n] ]
                     return (LL.fromList arblist)
                     -}
                  
    coarbitrary l
        | LL.null l = variant 0
        | otherwise = coarbitrary (LL.head l) . variant 1 .
                        coarbitrary (LL.tail l)

class (Arbitrary a, Show a, Eq a, Eq b, LL.ListLike a b) => TestLL a b where
    -- | Compare a ListLike to a list using any local conversions needed
    llcmp :: a -> [b] -> Bool
    llcmp f l = (LL.toList f) == l

    -- | Check the lenghts of the two items.  True if they should be considered
    -- to match.
    checkLengths :: a -> [b] -> Bool
    checkLengths f l = (LL.length f) == length l

instance (Arbitrary a, Show a, Eq a) => TestLL [a] a where
    llcmp x y = x == y

instance (Arbitrary a, Show a, Eq a) => TestLL (MyList a) a where
    llcmp (MyList x) l = x == l

instance TestLL BS.ByteString Word8 where

instance TestLL BSL.ByteString Word8 where

instance (Arbitrary a, Show a, Eq a) => TestLL (A.Array Int a) a where

instance (Show k, Show v, Arbitrary k, Arbitrary v, Ord v, Ord k) => TestLL (Map.Map k v) (k, v) where
    llcmp m l = mycmp (Map.toList m)
        where mycmp [] = True
              mycmp (x:xs) = if elem x l 
                                then mycmp xs
                                else False
    -- FIXME: should find a way to use LL.length instead of Map.size here
    checkLengths m l = Map.size m == length (mapRemoveDups l)

mapRemoveDups :: (Eq k1) => [(k1, v1)] -> [(k1, v1)]
mapRemoveDups = nubBy (\(k1, _) (k2, _) -> k1 == k2)

data MyList a = MyList [a]

instance (Show a) => Show (MyList a) where
    show (MyList x) = "MyList " ++ show x
instance (Eq a) => Eq (MyList a) where
    (==) (MyList x) (MyList y) = x == y

instance LL.FoldableLL (MyList a) a where
    foldr f i (MyList x) = foldr f i x
    foldl f i (MyList x) = foldl f i x
    foldr1 f (MyList x) = foldr1 f x
    foldl1 f (MyList x) = foldl1 f x

instance Monoid (MyList a) where
    mempty = MyList []
    mappend (MyList x) (MyList y) = MyList (x ++ y)

instance LL.ListLike (MyList a) a where
    singleton x = MyList [x]
    head (MyList x) = head x
    tail (MyList x) = MyList (tail x)
    null (MyList x) = null x

{-
instance (Ord a, Ord b, Show a, Show b, Arbitrary a, Arbitrary b) => Arbitrary (Map.Map a b) where
    arbitrary = fmap Map.fromList arbitrary
    coarbitrary a b = coarbitrary (Map.toList a) b
-}
instance Arbitrary Word8 where
    arbitrary = choose (0, maxBound)
    coarbitrary n = variant (2 * fromIntegral n)

{-
instance Arbitrary BS.ByteString where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n = 
                do arblist <- vector n
                   return (BS.pack arblist)
    coarbitrary bs
        | BS.null bs = variant 0
        | otherwise = coarbitrary (BS.head bs) . variant 1 . 
                        coarbitrary (BS.tail bs)

instance Arbitrary BSL.ByteString where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n = 
                do arblist <- vector n
                   return (BSL.pack arblist)
    coarbitrary bs
        | BSL.null bs = variant 0
        | otherwise = coarbitrary (BSL.head bs) . variant 1 . 
                        coarbitrary (BSL.tail bs)

instance (Arbitrary a) => Arbitrary (A.Array Int a) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return $ A.listArray (0, n - 1) arblist
    coarbitrary a = coarbitrary (A.elems a)
     
instance (Arbitrary a) => Arbitrary (MyList a) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (MyList arblist)
    coarbitrary (MyList x) = coarbitrary x
    -}

instance Random Word8 where
    randomR (a, b) g = (\(x, y) -> (fromInteger x, y)) $
                       randomR (toInteger a, toInteger b) g
    random g = randomR (minBound, maxBound) g

mkTest msg test = TestLabel msg $ TestCase $ (run test defOpt >>= checResult)
    where checResult (TestOk x y z) = printmsg x y >> return ()
          checResult (TestExausted x y z) = assertFailure (show (x, y, z))
          checResult (TestFailed x y) = assertFailure (show (x, y))
          checResult (TestAborted x) = assertFailure (show x)
          printmsg _ _ = return ()
          --printmsg x y = printf "\r%-78s\n" (msg ++ ": " ++ x ++ " (" ++ show y 
          --                            ++ " cases)")

data (LL.ListLike f i, Arbitrary f, Arbitrary i, TestLL f i, Eq i, Eq f) => LLTest f i = 
    forall t. Test.QuickCheck.Testable t => LLTest t

t :: String -> LLTest f i -> Test
t msg f = case f of
                    LLTest theTest -> mkTest msg theTest

instance Test.QuickCheck.Testable (LLTest f i) where
    property (LLTest x) = property x

w :: (Eq i, Eq f, TestLL f i, LL.ListLike f i, Arbitrary f, Arbitrary i) => String -> (forall t. Test.QuickCheck.Testable t => t) -> LLTest f i
w _ t = t

-- | all props, 3 args: full, full, and item
apf :: String -> (forall f i. (Eq i, Eq f, TestLL f i, LL.ListLike f i) => LLTest f i) -> Test 
apf msg x = TestLabel msg $ TestList $
    [t "[Int]" (x::LLTest [Int] Int),
     t "MyList Int" (x::LLTest (MyList Int) Int)
    ]
{-
     t "[Bool]" (x::[Bool] -> [Bool] -> Bool -> t),
     t "MyList Bool" (x::MyList Bool -> MyList Bool -> Bool -> t),
     t "Map Int Int" (x::Map.Map Int Int -> Map.Map Int Int -> (Int, Int) -> t),
     t "Map Bool Int" (x::Map.Map Bool Int -> Map.Map Bool Int -> (Bool, Int) -> t),
     t "Map Int Bool" (x::Map.Map Int Bool -> Map.Map Int Bool -> (Int, Bool) -> t),
     t "Map Bool Bool" (x::Map.Map Bool Bool -> Map.Map Bool Bool -> (Bool, Bool) -> t),
     t "ByteString" (x::BS.ByteString -> BS.ByteString -> Word8 -> t),
     t "ByteString.Lazy" (x::BSL.ByteString -> BSL.ByteString -> Word8 -> t),
     t "Array Int Int" (x::A.Array Int Int -> A.Array Int Int -> Int -> t),
     t "Array Int Bool" (x::A.Array Int Bool -> A.Array Int Bool -> Bool -> t)
    ]
-}
{-
-- | all props, 2 args: full and item
apfi :: Test.QuickCheck.Testable t => String -> (forall f i. (Eq i, Eq f, TestLL f i, LL.ListLike f i) => (f -> i -> t)) -> Test
apfi msg func = apffi msg newfunc
    where newfunc f1 _ i = func f1 i

-- | all props, 2 args: full, full
apff :: Test.QuickCheck.Testable t => String -> (forall f i. (Eq i, Eq f, TestLL f i, LL.ListLike f i) => (f -> f -> t)) -> Test
apff msg func = apffi msg newfunc
    where newfunc f1 f2 i = func f1 (asTypeOf f2 (LL.singleton i))

-- | all props, 1 arg: full
apf :: Test.QuickCheck.Testable t => String -> (forall f i. (Eq i, Eq f, TestLL f i, LL.ListLike f i) => (f -> t)) -> Test
apf msg func = 
    apfi msg newfunc
    where newfunc x y = func (asTypeOf x (LL.singleton y))
          -}
