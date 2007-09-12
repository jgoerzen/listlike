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
prop_cons f i = llcmp (LL.cons i f) (i : (LL.toList f))
prop_append f1 f2 = llcmp (LL.append f1 f2) (LL.toList f1 ++ LL.toList f2)
prop_head f = not (LL.null f) ==> LL.head f == head (LL.toList f)
prop_last f = not (LL.null f) ==> LL.last f == last (LL.toList f)
prop_tail f = not (LL.null f) ==> llcmp (LL.tail f) (tail (LL.toList f))
prop_init f = not (LL.null f) ==> llcmp (LL.init f) (init (LL.toList f))
prop_null f = LL.null f == null (LL.toList f)
prop_length2 f = checkLengths f (LL.toList f)
prop_length3 f1 f2 = llcmp (LL.append f1 f2) (LL.toList f1 ++ LL.toList f2)

allt = [apf "empty" prop_empty,
        apf "length" prop_length,
        apf "to/fromList" prop_tofromlist,
        apfi "singleton" prop_singleton,
        apfi "cons" prop_cons,
        apff "append" prop_append,
        apf "head" prop_head,
        apf "last" prop_last,
        apf "tail" prop_tail,
        apf "init" prop_init,
        apf "null" prop_null,
        apf "length2" prop_length2,
        apff "length3" prop_length3]

testh = runTestTT (TestList allt)

main = 
    do printf "Running %d test functions * %d types under test = %d cases\n"
              (length allt) (12::Int) (12 * length allt)
       testh 
       return ()

