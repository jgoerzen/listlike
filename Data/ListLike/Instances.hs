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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified System.IO as IO
import Data.Word

--------------------------------------------------
-- []

-- Basic list instance is in Base.hs
-- FoldableLL instance implied by Foldable

instance ListLikeIO String Char where
    hGetLine = IO.hGetLine
    hGetContents = IO.hGetContents
    -- FIXME: change to BSL
    hGet h c = BS.hGet h c >>= (return . toString)
    hGetNonBlocking h i = BS.hGetNonBlocking h i >>= (return . toString)
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
