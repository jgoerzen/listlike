{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.ListLike.Instances
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Instances of 'Data.ListLike.ListLike' and related classes.
Re-exported by "Data.ListLike".

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.ListLike.Instances () where
import Prelude hiding (length, head, last, null, tail, map, filter, concat, 
                       any, lookup, init, all, foldl, foldr, foldl1, foldr1,
                       maximum, minimum, iterate, span, break, takeWhile,
                       dropWhile, reverse, zip, zipWith, sequence,
                       sequence_, mapM, mapM_, concatMap, and, or, sum,
                       product, repeat, replicate, cycle, take, drop,
                       splitAt, elem, notElem, unzip, lines, words,
                       unlines, unwords)
import qualified Data.List as L
import Data.ListLike.Base
import Data.ListLike.String
import Data.ListLike.IO
import Data.ListLike.FoldableLL
import Data.Int
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Array.IArray as A
import Data.Array.IArray((!), (//), Ix(..))
import qualified Data.ByteString.Lazy as BSL
import qualified System.IO as IO
import Data.Word
import qualified Data.Map as Map

--------------------------------------------------
-- []

-- Basic list instance is in Base.hs
-- FoldableLL instance implied by Foldable

instance ListLikeIO String Char where
    hGetLine = IO.hGetLine
    hGetContents = IO.hGetContents
    hGet h c = BSL.hGet h c >>= (return . toString)
    hGetNonBlocking h i = BSL.hGetNonBlocking h i >>= (return . toString)
    hPutStr = IO.hPutStr
    hPutStrLn = IO.hPutStrLn
    getLine = IO.getLine
    getContents = IO.getContents
    putStr = IO.putStr
    putStrLn = IO.putStrLn
    interact = IO.interact
    readFile = IO.readFile
    writeFile = IO.writeFile

instance StringLike String where
    toString = id
    fromString = id

instance InfiniteListLike [a] a where
    iterate = L.iterate
    repeat = L.repeat
    cycle = L.cycle

--------------------------------------------------
-- ByteString

instance FoldableLL BS.ByteString Word8 where
    foldl = BS.foldl
    foldl' = BS.foldl'
    foldl1 = BS.foldl1
    foldr = BS.foldr
    foldr' = BS.foldr'
    foldr1 = BS.foldr1

instance ListLike BS.ByteString Word8 where
    empty = BS.empty
    singleton = BS.singleton
    cons = BS.cons
    snoc = BS.snoc
    append = BS.append
    head = BS.head
    last = BS.last
    tail = BS.tail
    init = BS.init
    null = BS.null
    length = BS.length
    -- map = BS.map
    rigidMap = BS.map
    reverse = BS.reverse
    intersperse = BS.intersperse
    concat = BS.concat . toList
    --concatMap = BS.concatMap
    rigidConcatMap = BS.concatMap
    any = BS.any
    all = BS.all
    maximum = BS.maximum
    minimum = BS.minimum
    replicate = BS.replicate
    take = BS.take
    drop = BS.drop
    splitAt = BS.splitAt
    takeWhile = BS.takeWhile
    dropWhile = BS.dropWhile
    span = BS.span
    break = BS.break
    group = fromList . BS.group
    inits = fromList . BS.inits
    tails = fromList . BS.tails
    isPrefixOf = BS.isPrefixOf
    isSuffixOf = BS.isSuffixOf
    --isInfixOf = BS.isInfixOf
    elem = BS.elem
    notElem = BS.notElem
    find = BS.find
    filter = BS.filter
    --partition = BS.partition
    index = BS.index
    elemIndex = BS.elemIndex
    elemIndices x = fromList . BS.elemIndices x
    findIndex = BS.findIndex
    findIndices x = fromList . BS.findIndices x
    --sequence = BS.sequence
    --mapM = BS.mapM
    --mapM_ = BS.mapM_
    --nub = BS.nub
    --delete = BS.delete
    --deleteFirsts = BS.deleteFirsts
    --union = BS.union
    --intersect = BS.intersect
    sort = BS.sort
    --insert = BS.insert
    toList = BS.unpack
    fromList = BS.pack
    fromListLike = fromList . toList
    --nubBy = BS.nubBy
    --deleteBy = BS.deleteBy
    --deleteFirstsBy = BS.deleteFirstsBy
    --unionBy = BS.unionBy
    --intersectBy = BS.intersectBy
    groupBy f = fromList . BS.groupBy f
    --sortBy = BS.sortBy
    --insertBy = BS.insertBy
    genericLength = fromInteger . fromIntegral . BS.length
    genericTake i = BS.take (fromIntegral i)
    genericDrop i = BS.drop (fromIntegral i)
    genericSplitAt i = BS.splitAt (fromIntegral i)
    genericReplicate i = BS.replicate (fromIntegral i)

instance ListLikeIO BS.ByteString Word8 where
    hGetLine = BS.hGetLine
    hGetContents = BS.hGetContents
    hGet = BS.hGet
    hGetNonBlocking = BS.hGetNonBlocking
    hPutStr = BS.hPutStr
    hPutStrLn = BS.hPutStrLn
    getLine = BS.getLine
    getContents = BS.getContents
    putStr = BS.putStr
    putStrLn = BS.putStrLn
    interact = BS.interact
    readFile = BS.readFile
    writeFile = BS.writeFile
    appendFile = BS.appendFile

instance StringLike BS.ByteString where
    toString = map (toEnum . fromIntegral) . BS.unpack
    fromString = BS.pack . map (fromIntegral . fromEnum)

--------------------------------------------------
-- ByteString.Lazy

instance FoldableLL BSL.ByteString Word8 where
    foldl = BSL.foldl
    foldl' = BSL.foldl'
    foldl1 = BSL.foldl1
    foldr = BSL.foldr
    --foldr' = BSL.foldr'
    foldr1 = BSL.foldr1

mi64toi :: Maybe Int64 -> Maybe Int
mi64toi Nothing = Nothing
mi64toi (Just x) = Just (fromIntegral x)

instance ListLike BSL.ByteString Word8 where
    empty = BSL.empty
    singleton = BSL.singleton
    cons = BSL.cons
    snoc = BSL.snoc
    append = BSL.append
    head = BSL.head
    last = BSL.last
    tail = BSL.tail
    init = BSL.init
    null = BSL.null
    length = fromIntegral . BSL.length
    -- map = BSL.map
    rigidMap = BSL.map
    reverse = BSL.reverse
    --intersperse = BSL.intersperse
    concat = BSL.concat . toList
    --concatMap = BSL.concatMap
    rigidConcatMap = BSL.concatMap
    any = BSL.any
    all = BSL.all
    maximum = BSL.maximum
    minimum = BSL.minimum
    replicate i = BSL.replicate (fromIntegral i)
    take i = BSL.take (fromIntegral i)
    drop i = BSL.drop (fromIntegral i)
    splitAt i = BSL.splitAt (fromIntegral i)
    takeWhile = BSL.takeWhile
    dropWhile = BSL.dropWhile
    span = BSL.span
    break = BSL.break
    group = fromList . BSL.group
    inits = fromList . BSL.inits
    tails = fromList . BSL.tails
    isPrefixOf = BSL.isPrefixOf
    --isSuffixOf = BSL.isSuffixOf
    --isInfixOf = BSL.isInfixOf
    elem = BSL.elem
    notElem = BSL.notElem
    find = BSL.find
    filter = BSL.filter
    --partition = BSL.partition
    index l i = BSL.index l (fromIntegral i)
    elemIndex i = mi64toi . BSL.elemIndex i 
    --elemIndices x = fromList . L.map fromIntegral . BSL.elemIndices x
    findIndex f = mi64toi . BSL.findIndex f
    --findIndices x = fromList . L.map fromIntegral . BSL.findIndices x
    --sequence = BSL.sequence
    --mapM = BSL.mapM
    --mapM_ = BSL.mapM_
    --nub = BSL.nub
    --delete = BSL.delete
    --deleteFirsts = BSL.deleteFirsts
    --union = BSL.union
    --intersect = BSL.intersect
    --sort = BSL.sort
    --insert = BSL.insert
    toList = BSL.unpack
    fromList = BSL.pack
    fromListLike = fromList . toList
    --nubBy = BSL.nubBy
    --deleteBy = BSL.deleteBy
    --deleteFirstsBy = BSL.deleteFirstsBy
    --unionBy = BSL.unionBy
    --intersectBy = BSL.intersectBy
    groupBy f = fromList . BSL.groupBy f
    --sortBy = BSL.sortBy
    --insertBy = BSL.insertBy
    genericLength = fromInteger . fromIntegral . BSL.length
    genericTake i = BSL.take (fromIntegral i)
    genericDrop i = BSL.drop (fromIntegral i)
    genericSplitAt i = BSL.splitAt (fromIntegral i)
    genericReplicate i = BSL.replicate (fromIntegral i)

strict2lazy :: BS.ByteString -> IO BSL.ByteString
strict2lazy b = return (BSL.fromChunks [b])
instance ListLikeIO BSL.ByteString Word8 where
    hGetLine h = BS.hGetLine h >>= strict2lazy
    hGetContents = BSL.hGetContents
    hGet = BSL.hGet
    hGetNonBlocking = BSL.hGetNonBlocking
    hPutStr = BSL.hPut
    --hPutStrLn = BSL.hPutStrLn
    getLine = BS.getLine >>= strict2lazy
    getContents = BSL.getContents
    putStr = BSL.putStr
    putStrLn = BSL.putStrLn
    interact = BSL.interact
    readFile = BSL.readFile
    writeFile = BSL.writeFile
    appendFile = BSL.appendFile

instance StringLike BSL.ByteString where
    toString = map (toEnum . fromIntegral) . BSL.unpack
    fromString = BSL.pack . map (fromIntegral . fromEnum)

--------------------------------------------------
-- Map

instance (Ord key) => FoldableLL (Map.Map key val) (key, val) where
    foldr f start m = Map.foldWithKey func start m
            where func k v accum = f (k, v) accum
    foldl f start m = Map.foldWithKey func start m
            where func k v accum = f accum (k, v)

l2m :: (Ord k, Ord k2) => ([(k, v)], [(k2, v2)]) -> (Map.Map k v, Map.Map k2 v2)
l2m (l1, l2) = (Map.fromList l1, Map.fromList l2)
instance (Ord key, Eq val) => ListLike (Map.Map key val) (key, val) where
    empty = Map.empty
    singleton (k, v) = Map.singleton k v
    cons (k, v) m = Map.insert k v m
    snoc = flip cons
    append = Map.union
    head = Map.elemAt 0
    last m = Map.elemAt (Map.size m - 1) m
    tail = Map.deleteAt 0
    init m = Map.deleteAt (Map.size m - 1) m
    null = Map.null
    length = Map.size
    map f = fromList . map f . Map.toList
    rigidMap f = Map.fromList . L.map f . Map.toList
    reverse = id
    intersperse = cons
    -- concat
    -- concatMap
    -- rigidConcatMap
    -- any
    -- all
    -- maximum
    -- minimum
    replicate _ = singleton
    take n = Map.fromAscList . L.take n . Map.toAscList
    drop n = Map.fromAscList . L.drop n . Map.toAscList
    splitAt n = l2m . L.splitAt n . Map.toList
    takeWhile f = Map.fromAscList . L.takeWhile f . Map.toAscList
    dropWhile f = Map.fromAscList . L.dropWhile f . Map.toAscList
    span f = l2m . L.span f . Map.toList
    break f = span (not . f)
    -- group
    -- inits
    -- tails
    -- isPrefixOf
    -- isSuffixOf
    isInfixOf = Map.isSubmapOf
    --elem = Map.member
    --notElem = Map.notMember
    -- find
    filter f m = Map.filterWithKey func m
            where func k v = f (k, v)
    index = flip Map.elemAt
    elemIndex (k, v) m =
        case Map.lookupIndex k m of
             Nothing -> fail "elemIndex: no matching key"
             Just i -> if snd (Map.elemAt i m) == v
                           then Just i
                           else fail "elemIndex on Map: matched key but not value"
    elemIndices i m = 
        case elemIndex i m of
             Nothing -> empty
             Just x -> singleton x
    -- findIndex
    -- findIndices
    -- sequence
    -- mapM
    -- rigidMapM
    -- mapM_
    nub = id
    delete (k, v) m =
        case Map.lookup k m of
             Nothing -> m
             Just x -> if x == v
                          then Map.delete k m
                          else m
    union = Map.union
    -- intersect
    sort = id
    insert = cons
    toList = Map.toList
    fromList = Map.fromList
    nubBy _ = id
    --deleteBy
    --deleteFirstsBy
    --unionBy
    --intersectBy
    --groupBy
    sortBy _ = id
    insertBy _ = insert
    genericLength = fromIntegral . Map.size
    genericTake n = Map.fromAscList . L.genericTake n . Map.toAscList
    genericDrop n = Map.fromAscList . L.genericDrop n . Map.toAscList
    genericSplitAt n = l2m . L.genericSplitAt n . Map.toList
    genericReplicate _ = singleton

--------------------------------------------------
-- Arrays

instance (Ix i) => FoldableLL (A.Array i e) e where
    foldl = F.foldl
    foldl1 = F.foldl1
    foldl' = F.foldl'
    foldr = F.foldr
    foldr1 = F.foldr1
    foldr' = F.foldr'

instance (Integral i, Ix i) => Monoid (A.Array i e) where
    mempty = A.listArray (0, -1) []
    mappend l1 l2 =
        A.array (blow, newbhigh)
              (A.assocs l1 ++ zip [(bhigh + 1)..newbhigh] (A.elems l2))
        where newlen = genericLength newelems
              newelems = A.elems l2
              newbhigh = bhigh + newlen
              (blow, bhigh) = A.bounds l1

instance (Integral i, Ix i) => ListLike (A.Array i e) e where
    empty = mempty
    singleton i = A.listArray (0, 0) [i]
    cons i l = 
        -- To add something to the beginning of an array, we must
        -- change the bounds and set the first element.
        (A.ixmap (blow - 1, bhigh) id l) // [(blow - 1, i)]
        where (blow, bhigh) = A.bounds l
    snoc l i = 
        -- Here we must change the bounds and set the last element
        (A.ixmap (blow, bhigh + 1) id l) // [(bhigh + 1, i)]
        where (blow, bhigh) = A.bounds l
    append = mappend
    head l = l ! (fst (A.bounds l))
    last l = l ! (snd (A.bounds l))
    tail l = A.array (blow + 1, bhigh) (tail (A.assocs l))
            where (blow, bhigh) = A.bounds l
    init l = A.array (blow, bhigh - 1) (init (A.assocs l))
            where (blow, bhigh) = A.bounds l
    null l = genericLength l == (0::Integer)
    length = genericLength
    -- map
    rigidMap = A.amap
    reverse l = A.listArray (A.bounds l) (L.reverse (A.elems l)) 
    -- intersperse
    -- concat
    -- concatMap
    -- rigidConcatMap
    any x = L.any x . A.elems
    all x = L.all x . A.elems
    maximum = L.maximum . A.elems
    minimum = L.minimum . A.elems
    replicate = genericReplicate
    take = genericTake
    drop = genericDrop
    -- splitAt
    -- takeWhile
    -- dropWhile
    -- span
    -- break
    -- group
    -- inits
    -- tails
    isPrefixOf l1 l2 = L.isPrefixOf (toList l1) (toList l2)
    isSuffixOf l1 l2 = L.isSuffixOf (toList l1) (toList l2)
    isInfixOf l1 l2 = L.isInfixOf (toList l1) (toList l2)
    elem i l = L.elem i (toList l)
    -- notElem
    filter f = fromList . L.filter f . toList
    -- partition
    index l i = l ! ((fromIntegral i) + offset)
        where offset = (fst $ A.bounds l)
    elemIndex i = L.elemIndex i . toList
    elemIndices i = fromList . L.elemIndices i . toList
    findIndex f = L.findIndex f . toList
    findIndices f = fromList . L.findIndices f . toList
    -- sequence = M.sequence . toList
    -- mapM f = M.mapM f . toList
    -- rigidMapM = mapM
    -- mapM_ f = M.mapM_ f . toList
    nub = fromList . L.nub . toList
    -- delete
    -- deleteFirsts
    -- union
    -- intersect
    sort l = A.listArray (A.bounds l) (L.sort (A.elems l))
    -- insert
    toList = A.elems
    fromList l = A.listArray (0, genericLength l - 1) l
    -- fromListLike
    nubBy f = fromList . L.nubBy f . toList
    -- deleteBy
    -- deleteFirstsBy
    -- unionBy
    -- intersectBy
    -- groupBy
    sortBy f l = A.listArray (A.bounds l) (L.sortBy f (A.elems l))
    -- insertBy
    genericLength l = fromIntegral (bhigh - blow + 1)
        where (blow, bhigh) = A.bounds l
    genericTake count l = A.listArray (blow, blow + (fromIntegral count))
                          (L.genericTake count (A.elems l))
        where (blow, _) = A.bounds l
    genericDrop count l = A.listArray (blow + (fromIntegral count), bhigh)
                          (L.genericDrop count (A.elems l))
        where (blow, bhigh) = A.bounds l
    -- geneicSplitAt
    genericReplicate count i = A.listArray (0, (fromIntegral count) - 1) 
                                           (L.genericReplicate count i)


instance (Integral i, Ix i) => StringLike (A.Array i Char) where
    toString = toList
    fromString = fromList
    -- lines
    -- words

instance (Integral i, Ix i) => ListLikeIO (A.Array i Char) Char where
    hGetLine h = IO.hGetLine h >>= (return . fromList)
    hGetContents h = IO.hGetContents h >>= (return . fromList)
    hGet h i = ((hGet h i)::IO String) >>= (return . fromList)
    hGetNonBlocking h i = ((hGetNonBlocking h i):: IO String) >>= (return . fromList)
    hPutStr h = hPutStr h . toString
    hPutStrLn h = hPutStrLn h . toString
    getLine = IO.getLine >>= (return . fromString)
    getContents = IO.getContents >>= (return . fromString)
    putStr = IO.putStr . toString
    putStrLn = IO.putStrLn . toString
    -- interact
    -- readFile
    -- writeFile
    -- appendFile
