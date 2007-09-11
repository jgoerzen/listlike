{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : tests
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Generic operations over list-like structures

Written by John Goerzen, jgoerzen\@complete.org

Please start with the introduction at "Data.ListLike#intro".
-}

import Test.QuickCheck
import Test.QuickCheck.Batch
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ListLike as LL
import qualified Data.Map as Map
import qualified Data.Array as A
import System.Random
import Test.HUnit
import Text.Printf
import Data.Word
import Data.List

class (Arbitrary b, Show b, LL.ListLike a b, Eq b) => TestLL a b where
    tl :: a -> [b]
    fl :: [b] -> a

    cl :: (a -> a) -> ([b] -> [b]) -> [b] -> Bool
    cl nativefunc listfunc listdata =
        tl (nativefunc (fl listdata)) == listfunc listdata

    {-
    tr :: String -> (a -> a) -> ([b] -> [b]) -> Test
    tr msg nativetest listtest =
        t msg (cl nativetest listtest)
    -}

instance (Arbitrary a, Show a, Eq a) => TestLL [a] a where
    tl = LL.toList
    fl = LL.fromList

instance (Arbitrary k, Show k, Show v, Arbitrary v, Ord v, Ord k, Eq v) => TestLL (Map.Map k v) (k, v) where
    fl = LL.fromList
    tl = LL.toList
    cl nativefunc listfunc listdata =
        (sort . tl . nativefunc . fl $ listdata) ==
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

instance Random Word8 where
    randomR (a, b) g = (\(x, y) -> (fromInteger x, y)) $
                       randomR (toInteger a, toInteger b) g
    random g = randomR (minBound, maxBound) g

{-
ta :: forall f l. (TestLL f l, LL.ListLike f l, Arbitrary f,
       Arbitrary l, Show l) => 
      String -> (f -> f) -> ([l] -> [l]) -> Test
-}
tr msg nativetest listtest =
    t msg (cl nativetest listtest)

ta msg nativetest listtest =
    TestList [tr (msg ++ " [Int]") nativetest listtest,
              tr (msg ++ " ByteString") nativetest listtest BS.empty]

                {-
              t (msg ++ " ByteString")
                (cl (nativetest::BS.ByteString -> BS.ByteString) listtest),
              t (msg ++ " ByteString.Lazy")
                (cl (nativetest::BSL.ByteString -> BSL.ByteString)
                    listtest),
              t (msg ++ " Map")
                (cl (nativetest::Map.Map Word8 Word8 -> Map.Map Word8 Word8) listtest),
              t (msg ++ " Array")
                (cl (nativetest::A.Array Int Word8 -> A.Array Int Word8) listtest)]

-}

prop_singleton :: Int -> Bool
prop_singleton i = LL.singleton i == [i]

allt = [--t "empty" prop_empty,
        t "singleton" prop_singleton]
        --ta "to/fromList" (LL.fromList . LL.toList) id]

{-
t msg test = runTests msg defOpt [run test]
testit = sequence_ allt
-}


t msg test = TestLabel msg $ TestCase $ (run test defOpt >>= checResult)
    where checResult (TestOk x y z) = printmsg x y >> return ()
          checResult (TestExausted x y z) = assertFailure (show (x, y, z))
          checResult (TestFailed x y) = assertFailure (show (x, y))
          checResult (TestAborted x) = assertFailure (show x)
          printmsg x y = printf "\r%-78s\n" (msg ++ ": " ++ x ++ " (" ++ show y 
                                      ++ " cases)")

--t msg test = TestLabel msg $ qctest msg test
testh = runTestTT (TestList allt)

main = testh

