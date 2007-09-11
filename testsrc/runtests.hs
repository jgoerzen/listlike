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

allt = 
       [ta "empty" (mktb (\() -> LL.empty) (\() -> []))
        --tase "empty2" (\_ -> LL.empty) (\_ -> []),
        --tase "singleton" LL.singleton (\x -> [x])
        --ta "singleton" (mktb LL.singleton (\x -> [x]))
        --ta "to/fromList" (LL.toList . LL.fromList) id 
        -- ta "cons" LL.cons (:)
       ]

testh = runTestTT (TestList allt)

main = testh

