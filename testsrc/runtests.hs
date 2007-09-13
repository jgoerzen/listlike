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
import qualified Test.HUnit as HU
import System.IO
import Text.Printf
import Data.Word
import Data.List
import Data.Monoid
import TestInfrastructure

-- prop_singleton :: (Eq i,LL.ListLike f i) => f -> i -> Bool
--prop_singleton :: (Eq i, LL.ListLike f i, Arbitrary f, Show f, Show i, Arbitrary i) => f -> i -> Bool
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

prop_map :: forall full item. (TestLL full item, TestLL [item] item) => full -> (item -> item) -> Result
prop_map f func = llcmp llmap (map func (LL.toList f))
    where llmap = asTypeOf (LL.map func f) (LL.toList f)

prop_rigidMap f func = llcmp (LL.rigidMap func f) (map func (LL.toList f))
prop_reverse f = llcmp (LL.reverse f) (reverse (LL.toList f))
prop_intersperse f i = llcmp (LL.intersperse i f) (intersperse i (LL.toList f))

prop_concat f = 
    llcmp (LL.concat f) (concat $ map LL.toList (LL.toList f))

prop_concatmap :: forall full item. (TestLL full item, TestLL [item] item) => full -> (item -> [item]) -> Result
prop_concatmap f func =
    llcmp (LL.concatMap func f)
          (concatMap func (LL.toList f))

prop_rigidConcatMap f func =
    llcmp (LL.rigidConcatMap func f)
          (concatMap (LL.toList . func) (LL.toList f))

prop_any f func = (LL.any func f) @?= (any func (LL.toList f))
prop_all f func = (LL.all func f) @?= (all func (LL.toList f))
prop_maximum f = not (LL.null f) ==> LL.maximum f @=? maximum (LL.toList f)
prop_minimum f = not (LL.null f) ==> LL.minimum f @=? minimum (LL.toList f)
prop_replicate f count i = 
    llcmp res (replicate count i)
    where res = asTypeOf (LL.replicate count i) f
prop_take f count = llcmp (LL.take count f) (take count (LL.toList f))
prop_drop f count = count >= 0 ==> llcmp (LL.drop count f) (drop count (LL.toList f))
prop_splitAt f count = count >= 0 ==>
    llcmp [(\(x, y) -> (LL.toList x, LL.toList y)) . LL.splitAt count $ f] 
          [LL.splitAt count (LL.toList f)]
prop_takeWhile f func = llcmp (LL.takeWhile func f) 
                              (takeWhile func (LL.toList f))
prop_dropWhile f func = llcmp (LL.dropWhile func f) 
                              (dropWhile func (LL.toList f))
prop_span f func = 
    llcmp [(\(x, y) -> (LL.toList x, LL.toList y)) . LL.span func $ f]
          [span func (LL.toList f)]
prop_break f func = 
    llcmp [(\(x, y) -> (LL.toList x, LL.toList y)) . LL.break func $ f]
          [break func (LL.toList f)]
prop_group f =
    -- llcmp (map LL.toList (LL.group f)) (group (LL.toList f))
    (map LL.toList (LL.group f)) @?= (group (LL.toList f))
prop_inits f = (map LL.toList (LL.inits f)) @?= (inits (LL.toList f))
prop_tails f = (map LL.toList (LL.tails f)) @?= (tails (LL.toList f))
prop_isPrefixOf f1 f2 = LL.isPrefixOf f1 f2 @?= 
    (isPrefixOf (LL.toList f1) (LL.toList f2))
prop_isSuffixOf f1 f2 = LL.isSuffixOf f1 f2 @?=
    (isSuffixOf (LL.toList f1) (LL.toList f2))
prop_isInfixOf f1 f2 = LL.isInfixOf f1 f2 @?=
    (isInfixOf (LL.toList f1) (LL.toList f2))
prop_elem f i = LL.elem i f @?= elem i (LL.toList f)
prop_notElem f i = LL.notElem i f @?= notElem i (LL.toList f)
prop_find f func = LL.find func f @?= find func (LL.toList f)
prop_filter f func = llcmp (LL.filter func f) (filter func (LL.toList f))
prop_partition f func = 
    (LL.toList f1, LL.toList f2) @?= partition func (LL.toList f)
    where (f1, f2) = LL.partition func f
prop_index f i = (i >= 0 && i < LL.length f) ==>
    (LL.index f i @?= ((LL.toList f) !! i))
prop_elemIndex f i = LL.elemIndex i f @?= elemIndex i (LL.toList f)
prop_elemIndices f i = LL.elemIndices i f @?= elemIndices i (LL.toList f)
prop_findIndex f func = LL.findIndex func f @?= findIndex func (LL.toList f)
prop_findIndices f func =
    LL.findIndices func f @?= findIndices func (LL.toList f)

prop_sequence f =
    case (llres, sequence testit) of
         (Just ll, Just l) -> llcmp ll l
         _ -> error "Error!"
    where testit = map Just (LL.toList f)
          llres = asTypeOf (LL.sequence testit) (Just f)

prop_mapM :: forall full item. (TestLL full item, TestLL [item] item) => full -> (item -> Maybe item) -> Result
prop_mapM f func = llmapM @?= (mapM func (LL.toList f))
    where llmapM = asTypeOf (LL.mapM func f) (Just (LL.toList f))

prop_rigidMapM :: forall full item. (TestLL full item, TestLL [item] item) => full -> (item -> Maybe item) -> Result
prop_rigidMapM f func = 
    case (LL.rigidMapM func f, mapM func (LL.toList f)) of
         (Just ll, Just l) -> llcmp ll l
         _ -> error "error in prop_rigidMapM"

allt = [apf "empty" (t prop_empty),
        apf "length" (t prop_length),
        apf "to/fromList" (t prop_tofromlist),
        apf "singleton" (t prop_singleton),
        apf "cons" (t prop_cons),
        apf "append" (t prop_append),
        apf "head" (t prop_head),
        apf "last" (t prop_last),
        apf "tail" (t prop_tail),
        apf "init" (t prop_init),
        apf "null" (t prop_null),
        apf "length2" (t prop_length2),
        apf "length3" (t prop_length3),
        apf "map" (t prop_map),
        apf "rigidMap" (t prop_rigidMap),
        apf "reverse" (t prop_reverse),
        apf "intersperse" (t prop_intersperse),
        apw "concat" (LLWrap prop_concat),
        apf "concatMap" (t prop_concatmap),
        apf "rigidConcatMap" (t prop_rigidConcatMap),
        apf "any" (t prop_any),
        apf "all" (t prop_all),
        apf "maximum" (t prop_maximum),
        apf "minimum" (t prop_minimum),
        apf "replicate" (t prop_replicate),
        apf "take" (t prop_take),
        apf "drop" (t prop_drop),
        apf "splitAt" (t prop_splitAt),
        apf "takeWhile" (t prop_takeWhile),
        apf "dropWhile" (t prop_dropWhile),
        apf "span" (t prop_span),
        apf "break" (t prop_break),
        apf "group" (t prop_group),
        apf "inits" (t prop_inits),
        apf "tails" (t prop_tails),
        apf "isPrefixOf" (t prop_isPrefixOf),
        apf "isSuffixOf" (t prop_isSuffixOf),
        apf "isInfixOf" (t prop_isInfixOf),
        apf "elem" (t prop_elem),
        apf "notElem" (t prop_notElem),
        apf "find" (t prop_find),
        apf "filter" (t prop_filter),
        apf "partition" (t prop_partition),
        apf "index" (t prop_index),
        apf "elemIndex" (t prop_elemIndex),
        apf "elemIndices" (t prop_elemIndices),
        apf "findIndex" (t prop_findIndex),
        apf "findIndices" (t prop_findIndices),
        apf "sequence" (t prop_sequence),
        apf "mapM" (t prop_mapM),
        apf "rigidMapM" (t prop_rigidMapM)
        ]

testh = HU.runTestTT (HU.TestList (reverse allt))

main = 
    do runVerbTestText (HU.putTextToHandle stderr True) (HU.TestList (reverse allt))
       return ()
