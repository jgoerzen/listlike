{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : ListLike
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
import FoldableLL
import qualified Control.Monad as M
import Data.Monoid
import qualified Data.Traversable as T
import qualified Data.ByteString as BS
import Data.Word
import qualified Data.Map as Map
import Data.Maybe

{- | The class implementing list-like functions.

Implementators must define at least:

* singleton
* head
* tail
* null or genericLength

-}
class (FoldableLL full item, Monoid full) =>
    ListLike full item | full -> item where

    ------------------------------ Creation
    {- | The empty list -}
    empty :: full
    empty = mempty

    {- | Creates a single-itement list out of an itement -}
    singleton :: item -> full

    ------------------------------ Basic Functions

    {- | Like (:) for lists: adds an itement to the beginning of a list -}
    cons :: item -> full -> full
    cons item l = append (singleton item) l

    {- | Adds an itement to the *end* of a 'ListLike'. -}
    snoc :: full -> item -> full
    snoc l item = append l (singleton item)

    {- | Combines two lists.  Like (++). -}
    append :: full -> full -> full 
    append = mappend

    {- | Extracts the first itement of a 'ListLike'. -}
    head :: full -> item

    {- | Extracts the last itement of a 'ListLike'. -}
    last :: full -> item
    last l = case genericLength l of
                  0 -> error "Called last on empty list"
                  1 -> head l
                  _ -> last (tail l)

    {- | Gives all itements after the head. -}
    tail :: full -> full 

    {- | All elements of the list except the last one. -}
    init :: full -> full
    init l
        | null l = error "init: empty list"
        | null xs = empty
        | otherwise = cons (head l) (init xs)
        where xs = tail l

    {- | Tests whether the list is empty. -}
    null :: full -> Bool
    null x = genericLength x == 0

    {- | Length of the list. -}
    length :: full -> Int
    length = genericLength

    ------------------------------ List Transformations

    {- | Apply a function to each element, returning any other
         valid 'ListLike'.  'rigidMap' will always be at least
         as fast, if not faster, than this function and is recommended
         if it will work for your purposes.  See also 'mapM'. -}
    map :: ListLike full' item' => (item -> item') -> full -> full'
    map func inp  
        | null inp = empty
        | otherwise = cons (func (head inp)) (map func (tail inp))

    {- | Like 'map', but without the possibility of changing the type of
       the item.  This can have performance benefits for things such as
       ByteStrings, since it will let the ByteString use its native
       low-level map implementation. -}
    rigidMap :: (item -> item) -> full -> full
    rigidMap = map

    {- | Reverse the elements in a list. -}
    reverse :: full -> full 
    reverse l = rev l empty
        where rev rl a
                | null rl = a
                | otherwise = rev (tail rl) (cons (head rl) a)
    {- | Add an item between each element in the structure -}
    intersperse :: item -> full -> full
    intersperse sep l
        | null l = empty
        | null xs = singleton x
        | otherwise = cons x (cons sep (intersperse sep xs))
        where x = head l
              xs = tail l

    ------------------------------ Reducing Lists (folds)
    -- See also functions in FoldableLLL

    ------------------------------ Special folds
    {- | Flatten the structure. -}
    concat :: (ListLike full' full, Monoid full) => full' -> full
    concat = fold

    {- | Map a function over the items and concatenate the results. -}
    concatMap :: (ListLike full' item') =>
                 (item -> full') -> full -> full'
    concatMap = foldMap

    {- | True if any items satisfy the function -}
    any :: (item -> Bool) -> full -> Bool
    any p = getAny . foldMap (Any . p)

    {- | True if all items satisfy the function -}
    all :: (item -> Bool) -> full -> Bool
    all p = getAll . foldMap (All . p)

    {- | The maximum value of the list -}
    maximum :: Ord item => full -> item
    maximum = foldr1 max

    {- | The minimum value of the list -}
    minimum :: Ord item => full -> item
    minimum = foldr1 min

    ------------------------------ Infinite lists
    {- | Generate a structure with the specified length with every element
    set to the item passed in.  See also 'genericReplicate' -}
    replicate :: Int -> item -> full
    replicate = genericReplicate

    ------------------------------ Sublists
    {- | Takes the first n elements of the list.  See also 'genericTake'. -}
    take :: Int -> full -> full
    take = genericTake

    {- | Drops the first n elements of the list.  See also 'genericDrop' -}
    drop :: Int -> full -> full
    drop = genericDrop

    {- | Equivalent to @('take' n xs, 'drop' n xs)@.  See also 'genericSplitAt'. -}
    splitAt :: Int -> full -> (full, full)
    splitAt = genericSplitAt

    {- | Returns all elements at start of list that satisfy the function. -}
    takeWhile :: (item -> Bool) -> full -> full
    takeWhile func l 
        | null l = empty
        | func x = cons x (takeWhile func (tail l))
        | otherwise = empty
        where x = head l

    {- | Drops all elements form the start of the list that satisfy the
       function. -}
    dropWhile :: (item -> Bool) -> full -> full
    dropWhile func l
        | null l = empty
        | func (head l) = dropWhile func (tail l)
        | otherwise = l

    {- | The equivalent of @('takeWhile' f xs, 'dropWhile' f xs)@ -}
    span :: (item -> Bool) -> full -> (full, full)
    span func l
        | null l = (empty, empty)
        | func x = (cons x ys, zs) 
        | otherwise = (empty, l)
       where (ys, zs) = span func (tail l)
             x = head l
    {- | The equivalent of @'span' ('not' . f)@ -}
    break :: (item -> Bool) -> full -> (full, full)
    break p = span (not . p)

    {- | Split a list into sublists, each which contains equal arguments.
       For order-preserving types, concatenating these sublists will produce
       the original list. See also 'groupBy'. -}
    group :: (ListLike full' full, Eq item) => full -> full'
    group = groupBy (==)

    {- | All initial segments of the list, shortest first -}
    inits :: (ListLike full' full) => full -> full'
    inits l
        | null l = singleton empty
        | otherwise =
            append (singleton empty)
                   (map (cons (head l)) theinits)
            where theinits = asTypeOf (inits (tail l)) [l]

    {- | All final segnemts, longest first -}
    tails :: ListLike full' full => full -> full'
    tails l
        | null l = singleton empty
        | otherwise = cons l (tails (tail l))

    ------------------------------ Predicates
    {- | True when the first list is at the beginning of the second. -}
    isPrefixOf :: Eq item => full -> full -> Bool
    isPrefixOf needle haystack
        | null needle = True
        | null haystack = False
        | otherwise = (head needle) == (head haystack) && 
                      isPrefixOf (tail needle) (tail haystack)

    {- | True when the first list is at the beginning of the second. -}
    isSuffixOf :: Eq item => full -> full -> Bool
    isSuffixOf needle haystack = isPrefixOf (reverse needle) (reverse haystack)

    {- | True when the first list is wholly containted within the second -}
    isInfixOf :: Eq item => full -> full -> Bool
    isInfixOf needle haystack = 
        any (isPrefixOf needle) thetails
        where thetails = asTypeOf (tails haystack) [haystack]

    ------------------------------ Searching
    {- | True if the item occurs in the list -}
    elem :: Eq item => item -> full -> Bool
    elem i = any (== i)

    {- | True if the item does not occur in the list -}
    notElem :: Eq item => item -> full -> Bool
    notElem i = all (/= i)

    {- | Take a function and return the first matching element, or Nothing
       if there is no such element. -}
    find :: (item -> Bool) -> full -> Maybe item
    find f l = case findIndex f l of
                    Nothing -> Nothing
                    Just x -> Just (index l x)

    {- | Returns only the elements that satisfy the function. -}
    filter :: (item -> Bool) -> full -> full 
    filter func l 
        | null l = empty
        | func (head l) = cons (head l) (filter func (tail l))
        | otherwise = filter func (tail l)

    {- | Returns the lists that do and do not satisfy the function.
       Same as @('filter' p xs, 'filter' ('not' . p) xs)@ -}
    partition :: (item -> Bool) -> full -> (full, full)
    partition p xs = (filter p xs, filter (not . p) xs)

    ------------------------------ Indexing
    {- | The element at 0-based index i.  Raises an exception if i is out
         of bounds.  Like (!!) for lists. -}
    index :: full -> Int -> item
    index l i 
        | null l = error "index: index not found"
        | i < 0 = error "index: index must be >= 0"
        | i == 0 = head l
        | otherwise = index (tail l) (i - 1)

    {- | Returns the index of the element, if it exists. -}
    elemIndex :: Eq item => item -> full -> Maybe Int
    elemIndex e l = findIndex (== e) l

    {- | Returns the indices of the matching elements.  See also 
       'findIndices' -}
    elemIndices :: (Eq item, ListLike result Int) => item -> full -> result
    elemIndices i l = findIndices (== i) l

    {- | Take a function and return the index of the first matching element,
         or Nothing if no element matches -}
    findIndex :: (item -> Bool) -> full -> Maybe Int
    findIndex f = listToMaybe . findIndices f

    {- | Returns the indices of all elements satisfying the function -}
    findIndices :: (ListLike result Int) => (item -> Bool) -> full -> result
    findIndices p xs = map snd $ filter (p . fst) $ thezips
        where thezips = asTypeOf (zip xs [0..]) [(head xs, 0::Int)]

    ------------------------------ Monadic operations
    {- | Evaluate each action in the sequence and collect the results -}
    sequence :: (Monad m, ListLike fullinp (m item)) =>
                fullinp -> m full
    sequence l = foldr func (return empty) l
        where func litem results = 
                do x <- litem
                   xs <- results
                   return (cons x xs)

    {- | A map in monad space.  Same as @'sequence' . 'map'@ -}
    mapM :: (Monad m, ListLike full' item') => 
            (item -> m item') -> full -> m full'
    mapM func l = sequence mapresult
            where mapresult = asTypeOf (map func l) []
            
    {- | A map in monad space, discarding results.  Same as
       @'sequence_' . 'map'@ -}
    mapM_ :: (Monad m) => (item -> m b) -> full -> m ()
    mapM_ func l = sequence_ mapresult
            where mapresult = asTypeOf (map func l) []


    ------------------------------ "Set" operations
    {- | Removes duplicate elements from the list.  See also 'nubBy' -}
    nub :: Eq item => full -> full
    nub = nubBy (==)

    {- | Removes the first instance of the element from the list.
       See also 'deleteBy' -}
    delete :: Eq item => item -> full -> full
    delete = deleteBy (==)

    {- | List difference.  Removes from the first list the first instance
       of each element of the second list.  See '(\\)' and 'deleteFirstsBy' -}
    deleteFirsts :: Eq item => full -> full -> full
    deleteFirsts = foldl (flip delete)

    {- | List union: the set of elements that occur in either list.
         Duplicate elements in the first list will remain duplicate.
         See also 'unionBy'. -}
    union :: Eq item => full -> full -> full
    union = unionBy (==)

    {- | List intersection: the set of elements that occur in both lists.
         See also 'intersectBy' -}
    intersect :: Eq item => full -> full -> full
    intersect = intersectBy (==)

    ------------------------------ Ordered lists
    {- | Sorts the list.  See also 'sortBy'. -}
    sort :: Ord item => full -> full
    sort = sortBy compare

    {- | Inserts the itement at the last place where it is still less than or
         equal to the next itement.  See also 'insertBy'. -}
    insert :: Ord item => item -> full -> full 
    insert = insertBy compare

    ------------------------------ Conversions

    {- | Converts the structure to a list.  This is logically equivolent
         to 'fromListLike', but may have a more optimized implementation. -}
    toList :: full -> [item]
    toList = fromListLike

    {- | Generates the structure from a list. -}
    fromList :: [item] -> full 
    fromList [] = empty
    fromList (x:xs) = cons x (fromList xs)

    {- | Converts one ListLike to another.  See also 'toList'.
         Default implementation is @fromListLike = map id@ -}
    fromListLike :: ListLike full' item => full -> full'
    fromListLike = map id

    ------------------------------ Generalized functions
    {- | Generic version of 'nub' -}
    nubBy :: (item -> item -> Bool) -> full -> full
    nubBy f l
        | null l = empty
        | otherwise =
            cons (head l) (nubBy f (filter (\y -> not (f (head l) y)) (tail l)))

    {- | Generic version of 'deleteBy' -}
    deleteBy :: (item -> item -> Bool) -> item -> full -> full
    deleteBy func i l
        | null l = empty
        | otherwise =
            if func i (head l)
               then tail l
               else cons (head l) (deleteBy func i (tail l))

    {- | Generic version of 'deleteFirsts' -}
    deleteFirstsBy :: (item -> item -> Bool) -> full -> full -> full
    deleteFirstsBy func = foldl (flip (deleteBy func))

    {- | Generic version of 'union' -}
    unionBy :: (item -> item -> Bool) -> full -> full -> full
    unionBy func x y =
        append x $ foldl (flip (deleteBy func)) (nubBy func y) x

    {- | Generic version of 'intersect' -}
    intersectBy :: (item -> item -> Bool) -> full -> full -> full
    intersectBy func xs ys = filter (\x -> any (func x) ys) xs

    {- | Generic version of 'group'. -}
    groupBy :: (ListLike full' full, Eq item) => 
                (item -> item -> Bool) -> full -> full'
    groupBy eq l
        | null l = empty
        | otherwise = cons (cons x ys) (groupBy eq zs)
                      where (ys, zs) = span (eq x) xs
                            x = head l
                            xs = tail l

    {- | Sort function taking a custom comparison function -}
    sortBy :: Ord item => (item -> item -> Ordering) -> full -> full 
    sortBy cmp = foldr (insertBy cmp) empty

    {- | Like 'insert', but with a custom comparison function -}
    insertBy :: Ord item => (item -> item -> Ordering) -> item ->
                full -> full 
    insertBy cmp x ys
        | null ys = singleton x
        | otherwise = case cmp x (head ys) of
                        GT -> cons (head ys) (insertBy cmp x (tail ys))
                        _ ->  cons x (tail ys)

    ------------------------------ Generic Operations
    {- | Length of the list -}
    genericLength :: Num a => full -> a
    genericLength l = calclen 0 l
        where calclen accum cl =
                  if null cl
                     then accum
                     else calclen (accum + 1) (tail cl)

    {- | Generic version of 'take' -}
    genericTake :: Integral a => a -> full -> full
    genericTake n l
        | n <= 0 = empty
        | null l = empty
        | otherwise = cons (head l) (genericTake (n - 1) (tail l))

    {- | Generic version of 'drop' -}
    genericDrop :: Integral a => a -> full -> full
    genericDrop n l 
        | n <= 0 = l
        | null l = l
        | otherwise = genericDrop (n - 1) (tail l)

    {- | Generic version of 'splitAt' -}
    genericSplitAt :: Integral a => a -> full -> (full, full)
    genericSplitAt n l = (genericTake n l, genericDrop n l)

    {- | Generic version of 'replicate' -}
    genericReplicate :: Integral a => a -> item -> full
    genericReplicate count x 
        | count < 0 = error "Replicate called with negative size"
        | otherwise = map (\_ -> x) [1..count]

{-
instance (ListLike full item) => Monad full where
    m >>= k = foldr (append . k) empty m
    m >> k = foldr (append . (\_ -> k)) empty m
    return x = singleton x
    fail _ = empty

instance (ListLike full item) => M.MonadPlus full where
    mzero = empty
    mplus = append
-}

{- | An extension to 'ListLike' for those data types that are similar
to a 'String'.  Minimal complete definition is 'toString' and
'fromString'. -}
class (ListLike full item) => StringLike full item | full -> item where
    {- | Converts the structure to a 'String' -}
    toString :: full -> String
    
    {- | Converts a 'String' to a list -}
    fromString :: String -> full

    {- | Breaks a string into a list of strings -}
    lines :: (ListLike full' full) => full -> full'
    --lines = map fromString . L.lines . toString 

    {- | Breaks a string into a list of words -}
    words :: ListLike full' full => full -> full'
    words = map fromString . L.words . toString

    {- | Joins lines -}
    unlines :: ListLike full' full => full' -> full
    unlines = fromString . L.unlines . map toString

    {- | Joins words -}
    unwords :: ListLike full' full => full' -> full
    unwords = fromString . L.unwords . map toString


{- | An extension to 'ListLike' for those data types that are capable
of dealing with infinite lists.  Some 'ListLike' functions are capable
of working with finite or infinite lists.  The functions here require
infinite list capability in order to work at all. -}
class (ListLike full item) => InfiniteListLike full item | full -> item where
    {- | An infinite list of repeated calls of the function to args -}
    iterate :: (item -> item) -> item -> full
    iterate f x = cons x (iterate f (f x))

    {- | An infinite list where each element is the same -}
    repeat :: item -> full
    repeat x = xs
        where xs = cons x xs

    {- | Converts a finite list into a circular one -}
    cycle :: full -> full
    cycle xs 
        | null xs = error "ListLike.cycle: empty list"
        | otherwise = xs' where xs' = append xs xs'

-- | Returns True if all elements are True
and :: ListLike full Bool => full -> Bool
and = all (== True)

-- | Returns True if any element is True
or :: ListLike full Bool => full -> Bool
or = any (== True)

-- | The sum of the list
sum :: (Num a, ListLike full a) => full -> a
sum = getSum . foldMap Sum

-- | The product of the list
product :: (Num a, ListLike full a) => full -> a
product = getProduct . foldMap Product

------------------------------ Zipping
{- | Takes two lists and returns a list of corresponding pairs. -}
zip :: (ListLike full item,
          ListLike fullb itemb,
          ListLike result (item, itemb)) =>
          full -> fullb -> result
zip = zipWith (\a b -> (a, b))

{- | Takes two lists and combines them with a custom combining function -}
zipWith :: (ListLike full item,
            ListLike fullb itemb,
            ListLike result resultitem) =>
            (item -> itemb -> resultitem) -> full -> fullb -> result
zipWith f a b
    | null a = empty
    | null b = empty
    | otherwise = cons (f (head a) (head b)) (zipWith f (tail a) (tail b))

{- | Converts a list of pairs into two separate lists of elements -}
unzip :: (ListLike full (itema, itemb),
          ListLike ra itema,
          ListLike rb itemb) => full -> (ra, rb)
unzip inp = foldr convert (empty, empty) inp
    where convert (a, b) (as, bs) = ((cons a as), (cons b bs))

{- | Evaluate each action, ignoring the results -}
sequence_ :: (Monad m, ListLike mfull (m item)) => mfull -> m ()
sequence_ l = foldr (>>) (return ()) l

instance ListLike [a] a where
    empty = []
    singleton x = [x]
    cons x l = x : l
    snoc l x = l ++ [x]
    append l1 l2 = l1 ++ l2
    head = L.head
    last = L.last
    tail = L.tail
    null = L.null
    length = L.length
    rigidMap = L.map
    reverse = L.reverse

instance StringLike String Char where
    toString = id
    fromString = id

instance ListLike BS.ByteString Word8 where
    empty = BS.empty
    singleton = BS.singleton
    append = BS.append
    head = BS.head
    tail = BS.tail
    rigidMap = BS.map

instance StringLike BS.ByteString Word8 where
    toString = map (toEnum . fromIntegral) . BS.unpack
    fromString = BS.pack . map (fromIntegral . fromEnum)

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

In most cases, functions here can act as drop-in replacements for their
list-specific counterparts.  They will use the same underlying implementations
for lists, so there should be no performance difference.
    
You can make your own types instances of 'ListLike' as well.  For more
details, see the notes for the 'ListLike' typeclass.
-}

