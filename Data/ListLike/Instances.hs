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
import qualified Data.ByteString as BS
import Data.Word

--------------------------------------------------
-- []

-- Basic list instance is in Base.hs
-- FoldableLL instance implied by Foldable

instance StringLike String where
    toString = id
    fromString = id

instance InfiniteListLike [a] a where
    iterate = L.iterate
    repeat = L.repeat
    cycle = L.cycle

instance ListLike BS.ByteString Word8 where
    empty = BS.empty
    singleton = BS.singleton
    append = BS.append
    head = BS.head
    tail = BS.tail
    rigidMap = BS.map

instance StringLike BS.ByteString where
    toString = map (toEnum . fromIntegral) . BS.unpack
    fromString = BS.pack . map (fromIntegral . fromEnum)
