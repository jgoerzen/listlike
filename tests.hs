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
import qualified Data.ListLike as LL
import Test.HUnit
import Test.HUnit.Utils
import Text.Printf

prop_empty :: Bool
prop_empty = LL.empty == ([]::[Int])

prop_singleton :: Int -> Bool
prop_singleton i = LL.singleton i == [i]

allt = [t "empty" prop_empty,
        t "singleton" prop_singleton]

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
testh = runTestTT (TestList (concat $ replicate 200 allt))

main = testh

