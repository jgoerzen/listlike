{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.ListLike
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Generic operations over list-like structures

Written by John Goerzen, jgoerzen\@complete.org
-}

module ListLike (-- * Introduction
                 -- $intro
                 -- * Creation & Basic Functions
                 empty, singleton, 
                 cons, snoc, append, head, last, tail, init, null, length,
                 -- ** Conversions
                 toList, fromList, fromListLike,
                 -- * Reducing lists (folds), from "FoldableLL"
                 foldl, foldl', foldl1, foldr, foldr', foldr1,
                 -- ** Special folds
                 concat, concatMap, 
                 and, or,
                 any, all,
                 sum, product,
                 maximum, minimum,
                 -- * Building lists
                 -- ** Scans
                 -- ** Accumulating maps
                 -- ** Infinite lists
                 iterate, repeat, replicate, cycle,
                 -- ** Unfolding
                 -- * Sublists
                 -- ** Extracting sublists
                 take, drop, splitAt, takeWhile, dropWhile, span, break,
                 group, inits, tails, 
                 -- ** Predicates
                 isPrefixOf, isSuffixOf, isInfixOf,
                 -- * Searching lists
                 -- ** Searching by equality
                 elem, notElem,
                 -- ** Searching with a predicate
                 find, filter, partition,
                 -- * Indexing lists
                 index, elemIndex, elemIndices, findIndex, findIndices,
                 -- * Zipping and unzipping lists
                 zip, zipWith, unzip,
                 -- * Monadic Operations
                 sequence, sequence_, mapM, mapM_,
                 -- * Special lists
                 -- ** Strings
                 toString, fromString, lines, words,
                 -- ** \"Set\" operations
                 nub, delete, deleteFirsts, union, intersect,
                 -- ** Ordered lists
                 sort, insert,
                 -- * Generalized functions
                 -- ** The \"By\" operations
                 -- *** User-supplied equality (replacing an Eq context)
                 nubBy, deleteBy, deleteFirstsBy, unionBy, intersectBy,
                 groupBy,
                 -- *** User-supplied comparison (replacing an Ord context)
                 sortBy, insertBy, -- maximumBy, minimumBy,
                 -- ** The \"generic\" operations
                 genericLength, genericTake, genericDrop, genericSplitAt,
                 -- genericIndex,
                 genericReplicate,
                 -- * The ListLike class
                 ListLike,
                 -- * The StringLike class
                 StringLike,
                 -- * The I\/O class
                 -- ListLikeIO,
                 -- * The InfiniteListLike class
                 InfiniteListLike
                )
       where
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
import Data.ListLike.FoldableLL
import Data.ListLike.Instances
import Data.ListLike.String
import Data.ListLike.Utils
import FoldableLL
import qualified Control.Monad as M
import Data.Monoid
import qualified Data.ByteString as BS
import Data.Word
import qualified Data.Map as Map
import Data.Maybe

{- $intro
Welcome to ListLike.

This module provides abstractions over typical list operations.
It is designed to let you freely interchange different ways to represent
sequences of data.  It works with lists, various types of ByteStrings,
and much more.

In this module, you'll find generic versions of most of the functions
you're used to using in the "Prelude" or from "Data.List".  They carry the
same names, too.  Therefore, you'll want to be careful how you import
the module.  I suggest using:

>import qualified ListLike as LL

Then, you can use LL.fold, LL.map, etc. to get the generic version of
the functions you want.  Alternatively, you can hide the other versions
from Prelude and import specific generic functions from here, such as:

>import Prelude hiding (map)
>import ListLike (map)

The module "Data.ListLike" actually simply re-exports the items found
in a number of its sub-modules.  If you want a smaller subset of
"Data.ListLike", look at the documentation for its sub-modules and import
the relevant one.

In most cases, functions here can act as drop-in replacements for their
list-specific counterparts.  They will use the same underlying implementations
for lists, so there should be no performance difference.
    
You can make your own types instances of 'ListLike' as well.  For more
details, see the notes for the 'ListLike' typeclass.
-}

