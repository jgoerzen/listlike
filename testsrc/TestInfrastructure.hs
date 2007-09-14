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
import System.IO
import qualified Test.HUnit as HU
import Text.Printf
import Data.Word
import Data.List
import Data.Monoid

{-
#if defined __HUGS__
-}
instance (Arbitrary a) => Arbitrary (Maybe a) where
  arbitrary            = sized arbMaybe
   where
    arbMaybe 0 = return Nothing
    arbMaybe n = fmap Just (resize (n-1) arbitrary)
  coarbitrary Nothing  = variant 0
  coarbitrary (Just x) = variant 1 . coarbitrary x
{-
#endif
-}

(@=?) :: (Eq a, Show a) => a -> a -> Result
expected @=? actual = 
        Result {ok = Just (expected == actual), 
                arguments = ["Result: expected " ++ show expected ++ ", got " ++ show actual],
                stamp = []}
    
(@?=) :: (Eq a, Show a) => a -> a -> Result
(@?=) = flip (@=?)

instance (LL.ListLike f i, Arbitrary i) => Arbitrary f where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    coarbitrary l = coarbitrary (LL.toList l)

class (Show b, Arbitrary a, Show a, Eq a, Eq b, LL.ListLike a b) => TestLL a b where
    -- | Compare a ListLike to a list using any local conversions needed
    llcmp :: a -> [b] -> Result
    llcmp f l = l @=? (LL.toList f)

    -- | Check the lenghts of the two items.  True if they should be considered
    -- to match.
    checkLengths :: a -> [b] -> Bool
    checkLengths f l = (LL.length f) == length l

instance (Arbitrary a, Show a, Eq a) => TestLL [a] a where
    llcmp x y = y @=? x

instance (Arbitrary a, Show a, Eq a) => TestLL (MyList a) a where
    llcmp (MyList x) l = l @=? x

instance TestLL BS.ByteString Word8 where

instance TestLL BSL.ByteString Word8 where

instance (Arbitrary a, Show a, Eq a) => TestLL (A.Array Int a) a where

instance (Show k, Show v, Arbitrary k, Arbitrary v, Ord v, Ord k) => TestLL (Map.Map k v) (k, v) where
    llcmp m l = 
        if mycmp (Map.toList m) && mychk l
            then l @=? l                         -- True
            else l @=? (Map.toList m)            -- False
        where mycmp [] = True
              mycmp (x:xs) = if elem x l 
                                then mycmp xs
                                else False
              mychk [] = True
              mychk ((k, _):xs) = if Map.member k m then mychk xs else False
    -- FIXME: should find a way to use LL.length instead of Map.size here
    checkLengths m l = Map.size m == length (mapRemoveDups l)

mapRemoveDups :: (Eq k1) => [(k1, v1)] -> [(k1, v1)]
mapRemoveDups = nubBy (\(k1, _) (k2, _) -> k1 == k2)

data MyList a = MyList [a]
    deriving (Ord, Eq, Show)

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

instance LL.StringLike (MyList Char) where
    toString (MyList x) = x
    fromString x = MyList x

instance Arbitrary Word8 where
    arbitrary = choose (0, maxBound)
    coarbitrary n = variant (2 * fromIntegral n)

instance Arbitrary Char where
    arbitrary = choose (toEnum 0, toEnum 255)
    coarbitrary n = variant (2 * fromIntegral (fromEnum n))

instance Random Word8 where
    randomR (a, b) g = (\(x, y) -> (fromInteger x, y)) $
                       randomR (toInteger a, toInteger b) g
    random g = randomR (minBound, maxBound) g

testoptions = defOpt {length_of_tests = 0, debug_tests = False}

mkTest msg test = HU.TestLabel msg $ HU.TestCase $ (run test testoptions >>= checResult)
    where checResult (TestOk x y z) = printmsg x y >> return ()
          checResult (TestExausted x y z) = 
            do hPrintf stderr "\r%-78s\n" $
                "Warning: Arguments exhausted after " ++ show y ++ " cases."
               return ()
          checResult (TestFailed x y) = HU.assertFailure $
                "Test Failure\n" ++ 
                "Arguments: " ++
                (concat . intersperse "\n           " $ x) ++ 
                "\nTest No.:  " ++ show y
          checResult (TestAborted x) = HU.assertFailure (show x)
          printmsg x y 
            | False = hPrintf stderr "\r%-78s\r" 
                      (msg ++ " " ++ x ++ " (" ++ show y ++ " cases)")
            | otherwise = return ()

-- Modified from HUnit
runVerbTestText :: HU.PutText st -> HU.Test -> IO (HU.Counts, st)
runVerbTestText (HU.PutText put us) t = do
  (counts, us') <- HU.performTest reportStart reportError reportFailure us t
  us'' <- put (HU.showCounts counts) True us'
  return (counts, us'')
 where
  reportStart ss us = do hPrintf stderr "\rTesting %-68s\n" (HU.showPath (HU.path ss))
                         put (HU.showCounts (HU.counts ss)) False us
  reportError   = reportProblem "Error:"   "Error in:   "
  reportFailure = reportProblem "Failure:" "Failure in: "
  reportProblem p0 p1 msg ss us = put line True us
   where line  = "### " ++ kind ++ path' ++ '\n' : msg
         kind  = if null path' then p0 else p1
         path' = HU.showPath (HU.path ss)


-- | So we can test map and friends
instance Show (a -> b) where
    show _ = "(a -> b)"

data (LL.ListLike f i, Arbitrary f, Arbitrary i, Show f, Show i, Eq i, Eq f) => LLTest f i = 
    forall t. Testable t => LLTest (f -> t)

data (LL.ListLike f i, Arbitrary f, Arbitrary i, Show f, Show i, Eq i, Eq f, LL.ListLike f' f, TestLL f' f, Show f', Eq f', Arbitrary f') =>
     LLWrap f' f i =
         forall t. Testable t => LLWrap (f' -> t)

w :: TestLL f i => String -> LLTest f i -> HU.Test
w msg f = case f of
                    LLTest theTest -> mkTest msg theTest

ws :: (LL.StringLike f, TestLL f i) => String -> LLTest f i -> HU.Test
ws = w

wwrap :: (TestLL f i, TestLL f' f) => String -> LLWrap f' f i -> HU.Test
wwrap msg f = case f of
                   LLWrap theTest -> mkTest msg theTest

t :: forall f t i. (TestLL f i, Arbitrary f, Arbitrary i, Show f, Eq f, Testable t) => (f -> t) -> LLTest f i
t = LLTest

-- | all props, wrapped list
apw :: String -> (forall f' f i. (TestLL f i, Show i, Eq i, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i, LL.ListLike f' f, Show f', TestLL f' f, Arbitrary f', Eq f') => LLWrap f' f i) -> HU.Test
apw msg x = HU.TestLabel msg $ HU.TestList $
    [wwrap "wrap [[Int]]" (x::LLWrap [[Int]] [Int] Int),
     wwrap "wrap MyList (MyList Int)" (x::LLWrap (MyList (MyList Int)) (MyList Int) Int),
     wwrap "wrap Array (Array Int)" (x::LLWrap (A.Array Int (A.Array Int Int)) (A.Array Int Int) Int),
     wwrap "wrap Array [Int]" (x::LLWrap (A.Array Int [Int]) [Int] Int)
     ]

-- | all props, 1 args: full
apf :: String -> (forall f i. (Ord i, TestLL f i, Show i, Eq i, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i) => LLTest f i) -> HU.Test 
apf msg x = HU.TestLabel msg $ HU.TestList $
    [w "[Int]" (x::LLTest [Int] Int),
     w "MyList Int" (x::LLTest (MyList Int) Int),
     w "String" (x::LLTest String Char),
     w "[Bool]" (x::LLTest [Bool] Bool),
     w "MyList Bool" (x::LLTest (MyList Bool) Bool),
     w "Map Int Int" (x::LLTest (Map.Map Int Int) (Int, Int)),
     w "Map Bool Int" (x::LLTest (Map.Map Bool Int) (Bool, Int)),
     w "Map Int Bool" (x::LLTest (Map.Map Int Bool) (Int, Bool)),
     w "Map Bool Bool" (x::LLTest (Map.Map Bool Bool) (Bool, Bool)),
     w "ByteString" (x::LLTest BS.ByteString Word8),
     w "ByteString.Lazy" (x::LLTest BSL.ByteString Word8),
     w "Array Int Int" (x::LLTest (A.Array Int Int) Int),
     w "Array Int Bool" (x::LLTest (A.Array Int Bool) Bool),
     w "[[Int]]" (x::LLTest [[Int]] [Int]),
     w "MyList (MyList Int)" (x::LLTest (MyList (MyList Int)) (MyList Int)),
     w "[MyList Int]" (x::LLTest [MyList Int] (MyList Int)),
     w "Array [Int]" (x::LLTest (A.Array Int [Int]) [Int]),
     w "Array (Array Int)" (x::LLTest (A.Array Int (A.Array Int Int)) (A.Array Int Int)),
     w "Array (Just Int)" (x::LLTest (A.Array Int (Maybe Int)) (Maybe Int))
    ]

-- | all props, 1 args: full
aps :: String -> (forall f i. (Ord i, TestLL f i, Show i, Eq i, LL.StringLike f, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i) => LLTest f i) -> HU.Test 
aps msg x = HU.TestLabel msg $ HU.TestList $
    [w "String" (x::LLTest String Char),
     w "MyList Char" (x::LLTest (MyList Char) Char),
     w "ByteString" (x::LLTest BS.ByteString Word8),
     w "ByteString.Lazy" (x::LLTest BSL.ByteString Word8),
     w "Array Int Char" (x::LLTest (A.Array Int Char) Char) 
    ]
