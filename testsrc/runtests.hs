{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

import Test.QuickCheck
import Test.QuickCheck.Batch
import qualified Data.ByteString as BS
import qualified Data.Array as A
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
import TestInfrastructure

-- prop_singleton :: (Eq i,LL.ListLike f i) => f -> i -> Bool
prop_singleton f x = (LL.toList $ asTypeOf (LL.singleton x) f) == [x]

prop_empty f = (LL.toList l == []) && (LL.null l) && (LL.length l == 0)
    where l = asTypeOf LL.empty f

prop_tofromlist f = 
    LL.toList f == l && 
    LL.length f == length l &&
    f == (LL.fromList . LL.toList $ f)
    where l = LL.toList f

prop_length f = LL.length f == length (LL.toList f)

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
    
allt = [apf "empty" prop_empty,
        apf "length" prop_length,
        apf "to/fromList" prop_tofromlist,
        apfi "singleton" prop_singleton]

testh = runTestTT (TestList allt)

main = 
    do printf "Running %d test functions * %d types under test = %d cases\n"
              (length allt) (12::Int) (12 * length allt)
       testh 
       return ()

