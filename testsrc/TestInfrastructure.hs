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

instance (Ord a, Ord b, Show a, Show b, Arbitrary a, Arbitrary b) => Arbitrary (Map.Map a b) where
    arbitrary = fmap Map.fromList arbitrary
    coarbitrary a b = coarbitrary (Map.toList a) b

instance Arbitrary Word8 where
    arbitrary = choose (0, maxBound)
    coarbitrary n = variant (2 * fromIntegral n)

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

instance Random Word8 where
    randomR (a, b) g = (\(x, y) -> (fromInteger x, y)) $
                       randomR (toInteger a, toInteger b) g
    random g = randomR (minBound, maxBound) g

t msg test = TestLabel msg $ TestCase $ (run test defOpt >>= checResult)
    where checResult (TestOk x y z) = printmsg x y >> return ()
          checResult (TestExausted x y z) = assertFailure (show (x, y, z))
          checResult (TestFailed x y) = assertFailure (show (x, y))
          checResult (TestAborted x) = assertFailure (show x)
          printmsg _ _ = return ()
          --printmsg x y = printf "\r%-78s\n" (msg ++ ": " ++ x ++ " (" ++ show y 
          --                            ++ " cases)")

-- | all props, 2 args: full and item
apfi :: String -> (forall f i. (Eq i, Eq f, LL.ListLike f i) => (f -> i -> Bool)) -> Test
apfi msg x = TestList $
    [t (msg ++ " [Int]") (x::[Int] -> Int -> Bool),
     t (msg ++ " MyList Int") (x::MyList Int -> Int -> Bool),
     t (msg ++ " [Bool]") (x::[Bool] -> Bool -> Bool),
     t (msg ++ " MyList Bool") (x::MyList Bool -> Bool -> Bool),
     t (msg ++ " Map Int Int") (x::Map.Map Int Int -> (Int, Int) -> Bool),
     t (msg ++ " Map Bool Int") (x::Map.Map Bool Int -> (Bool, Int) -> Bool),
     t (msg ++ " Map Int Bool") (x::Map.Map Int Bool -> (Int, Bool) -> Bool),
     t (msg ++ " Map Bool Bool") (x::Map.Map Bool Bool -> (Bool, Bool) -> Bool),
     t (msg ++ " ByteString") (x::BS.ByteString -> Word8 -> Bool),
     t (msg ++ " ByteString.Lazy") (x::BSL.ByteString -> Word8 -> Bool),
     t (msg ++ " Array Int Int") (x::A.Array Int Int -> Int -> Bool),
     t (msg ++ " Array Int Bool") (x::A.Array Int Bool -> Bool -> Bool)
    ]

-- | all props, 1 arg: full
apf :: String -> (forall f i. (Eq i, Eq f, LL.ListLike f i) => (f -> Bool)) -> Test
apf msg func = 
    apfi msg newfunc
    where newfunc x y = func (asTypeOf x (LL.singleton y))
    
