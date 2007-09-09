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

module Data.ListLike.Instances where
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
    hGet h c = BS.hGet h c >>= toString
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
    concat = BS.concat
    concatMap = BS.concatMap
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
    group = BS.group
    inits = BS.inits
    tails = BS.tails
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
    elemIndices = BS.elemIndices
    findIndex = BS.findIndex
    findIndices = BS.findIndices
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
    toList = toString
    fromList = fromString
    fromListLike = fromList . toList
    --nubBy = BS.nubBy
    --deleteBy = BS.deleteBy
    --deleteFirstsBy = BS.deleteFirstsBy
    --unionBy = BS.unionBy
    --intersectBy = BS.intersectBy
    groupBy = BS.groupBy
    --sortBy = BS.sortBy
    --insertBy = BS.insertBy
    genericLength = fromInteger . fromIntegral . BS.length
    genericTake i = BS.take (fromIntegral i)
    genericDrop i = BS.drop (fromIntegral i)
    genericSplitAt i = BS.splitAt (fromIntegral i)
    genericReplicate i = BS.replicate (fromIntegral i)

instance StringLike BS.ByteString where
    toString = map (toEnum . fromIntegral) . BS.unpack
    fromString = BS.pack . map (fromIntegral . fromEnum)
