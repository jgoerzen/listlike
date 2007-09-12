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
import qualified Test.HUnit as HU
import Text.Printf
import Data.Word
import Data.List
import Data.Monoid

(@=?) :: (Eq a, Show a) => a -> a -> Result
expected @=? actual = 
        Result {ok = Just (expected == actual), 
                arguments = ["Result: expected " ++ show expected ++ ", got " ++ show actual],
                stamp = []}
    
(@?=) :: (Eq a, Show a) => a -> a -> Result
(@?=) = flip (@=?)

instance (LL.ListLike f i, Arbitrary i) => Arbitrary f where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    coarbitrary l = coarbitrary (LL.toList l)

class (Show b, Arbitrary a, Show a, Eq a, Eq b, LL.ListLike a b) => TestLL a b where
    -- | Compare a ListLike to a list using any local conversions needed
    llcmp :: a -> [b] -> Result
    llcmp f l = l @=? (LL.toList f)

    -- | Check the lenghts of the two items.  True if they should be considered
    -- to match.
    checkLengths :: a -> [b] -> Bool
    checkLengths f l = (LL.length f) == length l

instance (Arbitrary a, Show a, Eq a) => TestLL [a] a where
    llcmp x y = y @=? x

instance (Arbitrary a, Show a, Eq a) => TestLL (MyList a) a where
    llcmp (MyList x) l = l @=? x

instance TestLL BS.ByteString Word8 where

instance TestLL BSL.ByteString Word8 where

instance (Arbitrary a, Show a, Eq a) => TestLL (A.Array Int a) a where

instance (Show k, Show v, Arbitrary k, Arbitrary v, Ord v, Ord k) => TestLL (Map.Map k v) (k, v) where
    llcmp m l = 
        if mycmp (Map.toList m)
            then l @=? l                         -- True
            else l @=? (Map.toList m)            -- False
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

instance Arbitrary Word8 where
    arbitrary = choose (0, maxBound)
    coarbitrary n = variant (2 * fromIntegral n)

instance Random Word8 where
    randomR (a, b) g = (\(x, y) -> (fromInteger x, y)) $
                       randomR (toInteger a, toInteger b) g
    random g = randomR (minBound, maxBound) g

mkTest msg test = HU.TestLabel msg $ HU.TestCase $ (run test defOpt >>= checResult)
    where checResult (TestOk x y z) = printmsg x y >> return ()
          checResult (TestExausted x y z) = HU.assertFailure (show (x, y, z))
          checResult (TestFailed x y) = HU.assertFailure $
                "Test Failure\n" ++ 
                "Arguments: " ++
                (concat . intersperse "\n           " $ x) ++ 
                "\nTest No.:  " ++ show y
          checResult (TestAborted x) = HU.assertFailure (show x)
          printmsg _ _ = return ()
          --printmsg x y = printf "\r%-78s\n" (msg ++ ": " ++ x ++ " (" ++ show y 
          --                            ++ " cases)")

-- | So we can test map and friends
instance Show (a -> b) where
    show _ = "(a -> b)"

data (LL.ListLike f i, Arbitrary f, Arbitrary i, Show f, Show i, Eq i, Eq f) => LLTest f i = 
    forall t. Testable t => LLTest (f -> t)

w :: TestLL f i => String -> LLTest f i -> HU.Test
w msg f = case f of
                    LLTest theTest -> mkTest msg theTest

t :: forall f t i. (TestLL f i, Arbitrary f, Arbitrary i, Show f, Eq f, Testable t) => (f -> t) -> LLTest f i
t = LLTest

apw :: forall f i t. (TestLL f i, Show i, Eq i, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i, Testable t) => String -> (forall f'. (LL.ListLike f' f, TestLL f' f, Show f', Eq f', Arbitrary f') => (f' -> t)) -> HU.Test 
apw msg x = HU.TestLabel msg $ HU.TestList $
    [w "wrap []" ((LLTest x)::LLTest [f] f),
     w "wrap MyList" ((LLTest x)::LLTest (MyList f) f)]
-- | all props, 3 args: full, full, and item
apf :: String -> (forall f i. (TestLL f i, Show i, Eq i, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i) => LLTest f i) -> HU.Test 
apf msg x = HU.TestLabel msg $ HU.TestList $
    [w "[Int]" (x::LLTest [Int] Int),
     w "MyList Int" (x::LLTest (MyList Int) Int),
     w "[Bool]" (x::LLTest [Bool] Bool),
     w "MyList Bool" (x::LLTest (MyList Bool) Bool),
     w "Map Int Int" (x::LLTest (Map.Map Int Int) (Int, Int)),
     w "Map Bool Int" (x::LLTest (Map.Map Bool Int) (Bool, Int)),
     w "Map Int Bool" (x::LLTest (Map.Map Int Bool) (Int, Bool)),
     w "Map Bool Bool" (x::LLTest (Map.Map Bool Bool) (Bool, Bool)),
     w "ByteString" (x::LLTest BS.ByteString Word8),
     w "ByteString.Lazy" (x::LLTest BSL.ByteString Word8),
     w "Array Int Int" (x::LLTest (A.Array Int Int) Int),
     w "Array Int Bool" (x::LLTest (A.Array Int Bool) Bool)
    ]
