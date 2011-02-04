{-# LANGUAGE ScopedTypeVariables
            ,RankNTypes
            ,ExistentialQuantification
            ,MultiParamTypeClasses
            ,FunctionalDependencies
            ,FlexibleInstances
            ,UndecidableInstances
            ,FlexibleContexts #-}

{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

-- FIXME -- better code is in offlineimap v7 branch
module TestInfrastructure where

import Test.QuickCheck
import Test.QuickCheck.Test
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ListLike as LL
import qualified Data.Array as A
import qualified Data.Foldable as F
import System.Random
import System.IO
import qualified Test.HUnit as HU
import Text.Printf
import Data.Word
import Data.List
import Data.Monoid


instance (Arbitrary i) => Arbitrary (MyList i) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink (MyList l) = map MyList $ shrink l

instance (CoArbitrary i) => CoArbitrary (MyList i) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary (BSL.ByteString) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance CoArbitrary (BSL.ByteString) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary (BS.ByteString) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance CoArbitrary (BS.ByteString) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary i => Arbitrary (A.Array Int i) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance (CoArbitrary i) => CoArbitrary (A.Array Int i) where
    coarbitrary l = coarbitrary (LL.toList l)

class (Show b, Arbitrary a, Show a, Eq a, Eq b, LL.ListLike a b) => TestLL a b where
  llcmp :: a -> [b] -> Property
  llcmp f l =  (putStrLn ("Expected: " ++ show l ++ "\nGot: " ++ show f))
               `whenFail` (l == (LL.toList f))
  checkLengths :: a -> [b] -> Bool
  checkLengths f l = (LL.length f) == length l

instance (Arbitrary a, Show a, Eq a) => TestLL [a] a where

instance (Arbitrary a, Show a, Eq a) => TestLL (MyList a) a where

instance TestLL BS.ByteString Word8 where

instance TestLL BSL.ByteString Word8 where

instance (Arbitrary a, Show a, Eq a) => TestLL (A.Array Int a) a where

mapRemoveDups :: (Eq k1) => [(k1, v1)] -> [(k1, v1)]
mapRemoveDups = nubBy (\(k1, _) (k2, _) -> k1 == k2)

data MyList a = MyList [a]
    deriving (Ord, Eq, Show)

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

instance LL.StringLike (MyList Char) where
    toString (MyList x) = x
    fromString x = MyList x

mkTest msg test = HU.TestLabel msg $ HU.TestCase (quickCheck test)

-- Modified from HUnit
runVerbTestText :: HU.PutText st -> HU.Test -> IO (HU.Counts, st)
runVerbTestText (HU.PutText put us) t = do
  (counts, us') <- HU.performTest reportStart reportError reportFailure us t
  us'' <- put (HU.showCounts counts) True us'
  return (counts, us'')
 where
  reportStart ss us = do hPrintf stderr "\rTesting %-68s\n" (HU.showPath (HU.path ss))
                         put (HU.showCounts (HU.counts ss)) False us
  reportError   = reportProblem "Error:"   "Error in:   "
  reportFailure = reportProblem "Failure:" "Failure in: "
  reportProblem p0 p1 msg ss us = put line True us
   where line  = "### " ++ kind ++ path' ++ '\n' : msg
         kind  = if null path' then p0 else p1
         path' = HU.showPath (HU.path ss)


-- | So we can test map and friends
instance Show (a -> b) where
    show _ = "(a -> b)"

data (LL.ListLike f i, Arbitrary f, Arbitrary i, Show f, Show i, Eq i, Eq f) => LLTest f i = 
    forall t. Testable t => LLTest (f -> t)

data (LL.ListLike f i, Arbitrary f, Arbitrary i, Show f, Show i, Eq i, Eq f, LL.ListLike f' f, TestLL f' f, Show f', Eq f', Arbitrary f') =>
     LLWrap f' f i =
         forall t. Testable t => LLWrap (f' -> t)

w :: TestLL f i => String -> LLTest f i -> HU.Test
w msg f = case f of
                    LLTest theTest -> mkTest msg theTest

ws :: (LL.StringLike f, TestLL f i) => String -> LLTest f i -> HU.Test
ws = w

wwrap :: (TestLL f i, TestLL f' f) => String -> LLWrap f' f i -> HU.Test
wwrap msg f = case f of
                   LLWrap theTest -> mkTest msg theTest

t :: forall f t i. (TestLL f i, Arbitrary f, Arbitrary i, Show f, Eq f, Testable t) => (f -> t) -> LLTest f i
t = LLTest

-- | all props, wrapped list
apw :: String -> (forall f' f i. (TestLL f i, Show i, Eq i, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i, LL.ListLike f' f, Show f', TestLL f' f, Arbitrary f', Eq f') => LLWrap f' f i) -> HU.Test
apw msg x = HU.TestLabel msg $ HU.TestList $
    [wwrap "wrap [[Int]]" (x::LLWrap [[Int]] [Int] Int),
     wwrap "wrap MyList (MyList Int)" (x::LLWrap (MyList (MyList Int)) (MyList Int) Int),
     wwrap "wrap Array (Array Int)" (x::LLWrap (A.Array Int (A.Array Int Int)) (A.Array Int Int) Int),
     wwrap "wrap Array [Int]" (x::LLWrap (A.Array Int [Int]) [Int] Int)
     ]

-- | all props, 1 args: full
apf :: String -> (forall f i. (Ord i, TestLL f i, Show i, Eq i, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i, CoArbitrary f, CoArbitrary i) => LLTest f i) -> HU.Test 
apf msg x = HU.TestLabel msg $ HU.TestList $
    [w "[Int]" (x::LLTest [Int] Int),
     w "MyList Int" (x::LLTest (MyList Int) Int),
     w "String" (x::LLTest String Char),
     w "[Bool]" (x::LLTest [Bool] Bool),
     w "MyList Bool" (x::LLTest (MyList Bool) Bool),
     w "ByteString" (x::LLTest BS.ByteString Word8),
     w "ByteString.Lazy" (x::LLTest BSL.ByteString Word8),
     w "Array Int Int" (x::LLTest (A.Array Int Int) Int),
     w "Array Int Bool" (x::LLTest (A.Array Int Bool) Bool),
     w "Array (Just Int)" (x::LLTest (A.Array Int (Maybe Int)) (Maybe Int))
    ]

-- | all props, 1 args: full
aps :: String -> (forall f i. (Ord i, TestLL f i, Show i, Eq i, LL.StringLike f, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i) => LLTest f i) -> HU.Test 
aps msg x = HU.TestLabel msg $ HU.TestList $
    [w "String" (x::LLTest String Char),
     w "MyList Char" (x::LLTest (MyList Char) Char),
     w "ByteString" (x::LLTest BS.ByteString Word8),
     w "ByteString.Lazy" (x::LLTest BSL.ByteString Word8),
     w "Array Int Char" (x::LLTest (A.Array Int Char) Char) 
    ]
