import Test.QuickCheck
import Test.QuickCheck.Batch
import Text.Printf
import Test.HUnit
import qualified Data.ListLike as LL

prop_rev :: Eq a => [a] -> Bool
prop_rev xs = reverse (reverse xs) == xs

tests = sequence_ [Test.QuickCheck.test (prop_rev::[Int]->Bool),
                   Test.QuickCheck.test (prop_rev::[Integer]->Bool)]

tests2 = [t "1" (prop_rev::[Int] -> Bool),
          t "2" (prop_rev::[Integer] -> Bool)]

data TestFunc = forall a b. LL.ListLike a b => MkT (a -> a)
wtf :: forall b. TestFunc -> (forall a a'. LL.ListLike a a' => (a -> a) -> b) -> b
wtf s f = case s of
               MkT x -> f x

data TestData = forall a b. (Arbitrary a, Show a, Show b, Arbitrary b, LL.ListLike a b) => MkD a
wtf2 :: forall b. TestData -> (forall a a'. (Arbitrary a, Show a, LL.ListLike a a') => a -> b) -> b
wtf2 s f = case s of
               MkD x -> f x

items = [MkD ([]::[Int]), MkD ([]::[Integer])]

tests3 f = map thetest items
    where thetest item = wtf2 item (genit f)
          
genit f c = t "test" f
    where typed = (f c)::Bool

t msg test = TestLabel msg $ TestCase $ (run test defOpt >>= checResult)
    where checResult (TestOk x y z) = printmsg x y >> return ()
          checResult (TestExausted x y z) = assertFailure (show (x, y, z))
          checResult (TestFailed x y) = assertFailure (show (x, y))
          checResult (TestAborted x) = assertFailure (show x)
          printmsg x y = printf "\r%-78s\n" (msg ++ ": " ++ x ++ " (" ++ show y 
                                      ++ " cases)")

