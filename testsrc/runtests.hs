{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

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
import TestInfrastructure

prop_singleton :: (Eq i,LL.ListLike f i) => f -> i -> Bool
prop_singleton f x = (LL.toList $ asTypeOf (LL.singleton x) f) == [x]

allprops :: (forall f i. (Eq i, LL.ListLike f i) => (f -> i -> Bool)) -> Test
allprops x = TestList $
    [t "singleton" $ x (LL.empty::[Int]),
     t "singleton" $ x (LL.empty::MyList Int)]
    
allt = map (t2 "empty") (ta (\_ -> LL.fromList []) (\_ -> []))
        -- tase "empty2" (\_ -> LL.empty) (\_ -> []),
        -- tase "singleton" LL.singleton (\x -> [x]),
        ++ map (t2 "to/fromList") (ta (LL.fromList . LL.toList) id)
        --ta "cons" LL.cons (:)
        ++ [allprops prop_singleton]

testh = runTestTT (TestList allt)

main = testh

