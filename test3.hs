import Test.QuickCheck
import Test.QuickCheck.Batch
import Text.Printf
import Test.HUnit
import qualified Data.ListLike as LL

prop_rev :: (Eq a, LL.ListLike a b) => a -> Bool
prop_rev xs = LL.reverse (LL.reverse xs) == xs

tests = sequence_ [Test.QuickCheck.test (prop_rev::[Int]->Bool),
                   Test.QuickCheck.test (prop_rev::[Integer]->Bool)]

tests2 = [t "1" (prop_rev::[Int] -> Bool),
          t "2" (prop_rev::[Integer] -> Bool)]

{-
test3 :: (Eq a, LL.ListLike a b) => (a -> Bool) -> Bool
test3 f = f ([1, 2]::[Int])
          -- f [1, 2::Int]]
-}

test4 :: (forall a. Eq a => (a -> a -> Bool)) -> [Bool]
test4 f = [f 1 (2::Int), f 5 (5::Integer)]
           

t msg test = TestLabel msg $ TestCase $ (run test defOpt >>= checResult)
    where checResult (TestOk x y z) = printmsg x y >> return ()
          checResult (TestExausted x y z) = assertFailure (show (x, y, z))
          checResult (TestFailed x y) = assertFailure (show (x, y))
          checResult (TestAborted x) = assertFailure (show x)
          printmsg x y = printf "\r%-78s\n" (msg ++ ": " ++ x ++ " (" ++ show y 
                                      ++ " cases)")

