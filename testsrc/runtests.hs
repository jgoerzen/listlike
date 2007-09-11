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

prop_empty f x = (LL.toList l == []) && (LL.null l) && (LL.length l == 0)
    where l = asTypeOf LL.empty f

prop_tofromlist f x = 
    LL.toList f == l && 
    LL.length f == length l &&
    f == (LL.fromList . LL.toList $ f)
    where l = asTypeOf (LL.toList f) [x]

prop_length f x =
    LL.length f == length l
    where l = asTypeOf (LL.toList f) [x]

-- | all props, 2 args: full and item
apfi :: String -> (forall f i. (Eq i, Eq f, LL.ListLike f i) => (f -> i -> Bool)) -> Test
apfi msg x = TestList $
    [t (msg ++ " [Int]") $ x (LL.empty::[Int]),
     t (msg ++ " MyList Int") $ x (LL.empty::MyList Int),
     t (msg ++ " [Bool]") $ x (LL.empty::[Bool]),
     t (msg ++ " MyList Bool") $ x (LL.empty::MyList Bool),
     t (msg ++ " Map Int Int") $ x (LL.empty::Map.Map Int Int),
     t (msg ++ " Map Bool Int") $ x (LL.empty::Map.Map Bool Int),
     t (msg ++ " Map Int Bool") $ x (LL.empty::Map.Map Int Bool),
     t (msg ++ " Map Bool Bool") $ x (LL.empty::Map.Map Bool Bool),
     t (msg ++ " ByteString") $ x (LL.empty::BS.ByteString),
     t (msg ++ " ByteString.Lazy") $ x (LL.empty::BSL.ByteString),
     t (msg ++ " Array Int Int") $ x (LL.empty::A.Array Int Int),
     t (msg ++ " Array Int Bool") $ x (LL.empty::A.Array Int Bool)]

    
allt = [apfi "empty" prop_empty,
        apfi "length" prop_length,
        apfi "to/fromList" prop_tofromlist,
    -- map (t2 "empty") (ta (\_ -> LL.fromList []) (\_ -> []))
        -- tase "empty2" (\_ -> LL.empty) (\_ -> []),
        -- tase "singleton" LL.singleton (\x -> [x]),
        -- ++ map (t2 "to/fromList") (ta (LL.fromList . LL.toList) id)
        --ta "cons" LL.cons (:)
        apfi "singleton" prop_singleton]

testh = runTestTT (TestList allt)

main = testh

