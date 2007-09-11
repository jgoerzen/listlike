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

class (Arbitrary b, Show b, LL.ListLike a b, Eq b) => TestLL a b where
    tl :: a -> [b]
    fl :: [b] -> a

    cl :: (a -> a) -> ([b] -> [b]) -> [b] -> Bool
    cl nativefunc listfunc listdata =
        tl (nativefunc (fl listdata)) == listfunc listdata

instance (Arbitrary a, Show a, Eq a) => TestLL [a] a where
    tl = LL.toList
    fl = LL.fromList

instance (Arbitrary a, Show a, Eq a) => TestLL (MyList a) a where
    tl = LL.toList
    fl = LL.fromList

instance (Arbitrary k, Show k, Show v, Arbitrary v, Ord v, Ord k, Eq v) => TestLL (Map.Map k v) (k, v) where
    fl = LL.fromList
    tl = LL.toList
    cl nativefunc listfunc listdata =
        (sort . tl . nativefunc . Map.fromList $ listdata) ==
        (sort . convl . listfunc . convl $ listdata)
        where convl = foldl myinsert [] 
              myinsert [] newval = [newval]
              myinsert ((ak, av):as) (nk, nv)
                | ak == nk = (nk, nv) : as
                | otherwise = (ak, av) : myinsert as (nk, nv)
                  
instance TestLL BS.ByteString Word8 where
    tl = LL.toList
    fl = LL.fromList

instance TestLL BSL.ByteString Word8 where
    tl = LL.toList
    fl = LL.fromList

instance TestLL (A.Array Int Int) Int where
    tl = LL.toList
    fl = LL.fromList

instance Arbitrary (Map.Map Int Int) where
    arbitrary = fmap fl arbitrary
    coarbitrary a b = coarbitrary (tl a) b

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

instance Arbitrary (A.Array Int Int) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return $ A.listArray (0, n - 1) arblist
    coarbitrary a = coarbitrary (A.elems a)
     
instance Arbitrary (MyList Int) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (MyList arblist)
    coarbitrary (MyList x) = coarbitrary x

instance Random Word8 where
    randomR (a, b) g = (\(x, y) -> (fromInteger x, y)) $
                       randomR (toInteger a, toInteger b) g
    random g = randomR (minBound, maxBound) g

tr msg nativetest listtest =
    t msg (cl nativetest listtest)

{- | Test with All types, No Convert -}
tanc :: forall l. (Eq l, Arbitrary l, Show l) => 
      String -> (forall f x. (TestLL f l, LL.ListLike f l, Arbitrary f) 
                 => (x -> f)) 
             -> (forall z. (z -> [l])) 
             -> Test
tanc msg nativetest listtest = 
    TestList 
    [t (msg ++ " [Int]") 
       (\(input::[Int]) -> fl (nativetest input) == listtest input),
     t (msg ++ " MyList Int")
       (\(input::MyList Int) -> fl (nativetest input) == listtest input)]
    {-
     t (msg ++ " MyList Int") (cl (nativetest::(MyList Int -> MyList Int)) listtest),
     t (msg ++ " Map") (cl (nativetest::(Map.Map Int Int -> Map.Map Int Int)) listtest),
     t (msg ++ " ByteString") (cl (nativetest::(BS.ByteString -> BS.ByteString)) listtest),
     t (msg ++ " ByteString.Lazy") (cl (nativetest::(BSL.ByteString -> BSL.ByteString)) listtest),
     t (msg ++ " Array") (cl (nativetest::(A.Array Int Int -> A.Array Int Int)) listtest)]
     -}

{- | Test with All types. -}
ta :: 
      String -> (forall f i. (TestLL f i, LL.ListLike f i, Arbitrary f,
                 Arbitrary i, Show i) => (f -> f)) 
             -> (forall l. (Arbitrary l, Show l) => ([l] -> [l])) 
             -> Test
ta msg nativetest listtest = 
    TestList 
    [t (msg ++ " [Int]") (cl (nativetest::([Int] -> [Int])) listtest),
     t (msg ++ " MyList Int") (cl (nativetest::(MyList Int -> MyList Int)) listtest),
     t (msg ++ " Map") (cl (nativetest::(Map.Map Int Int -> Map.Map Int Int)) listtest),
     t (msg ++ " ByteString") (cl (nativetest::(BS.ByteString -> BS.ByteString)) listtest),
     t (msg ++ " ByteString.Lazy") (cl (nativetest::(BSL.ByteString -> BSL.ByteString)) listtest),
     t (msg ++ " Array") (cl (nativetest::(A.Array Int Int -> A.Array Int Int)) listtest)]

t msg test = TestLabel msg $ TestCase $ (run test defOpt >>= checResult)
    where checResult (TestOk x y z) = printmsg x y >> return ()
          checResult (TestExausted x y z) = assertFailure (show (x, y, z))
          checResult (TestFailed x y) = assertFailure (show (x, y))
          checResult (TestAborted x) = assertFailure (show x)
          printmsg x y = printf "\r%-78s\n" (msg ++ ": " ++ x ++ " (" ++ show y 
                                      ++ " cases)")

