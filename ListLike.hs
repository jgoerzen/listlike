{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the University nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
-}

{- |
   Module     : ListLike
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : BSD

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with strings.

Written by John Goerzen, jgoerzen\@complete.org
-}

module ListLike where
import Prelude hiding (length, head, last, null, tail, map, filter, concat, 
                       any, lookup)
import qualified Data.List as L
import qualified FoldableLL as F
import qualified Control.Monad as M
import Data.Monoid
import qualified Data.Traversable as T
import qualified Data.ByteString as BS
import Data.Word
import qualified Data.Map as Map

{- | The class implementing list-like functions.

Implementators must define at least:

* singleton
* head
* tail
* null or genericLength

-}
class (F.FoldableLL full item, Monoid full) =>
    ListLike full item | full -> item where
    {- | The empty list -}
    empty :: full
    empty = mempty

    {- | Creates a single-itement list out of an itement -}
    singleton :: item -> full

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
                  0 -> error "Called last on empty item"
                  1 -> head l
                  _ -> last (tail l)

    {- | Gives all itements after the head. -}
    tail :: full -> full 

    {- | Tests whether the list is empty. -}
    null :: full -> Bool
    null x = genericLength x == 0

    {- | Length of the list. -}
    length :: full -> Int
    length = genericLength

    {- | Length of the list -}
    genericLength :: Num a => full -> a
    genericLength l = calclen 0 l
        where calclen accum cl =
                  if null cl
                     then accum
                     else calclen (accum + 1) (tail cl)

    {- | Apply a function to each element, returning any other
         valid 'ListLike'.  'rigidMap' will always be at least
         as fast, if not faster, than this function and is recommended
         if it will work for your purposes. -}
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

    {- | Sorts the list. -}
    sort :: Ord item => full -> full
    sort = sortBy compare

    {- | Sort function taking a custom comparison function -}
    sortBy :: Ord item => (item -> item -> Ordering) -> full -> full 
    sortBy cmp = F.foldr (insertBy cmp) empty

    {- | Inserts the itement at the last place where it is still less than or
         equal to the next itement -}
    insert :: Ord item => item -> full -> full 
    insert = insertBy compare

    {- | Like 'insert', but with a custom comparison function -}
    insertBy :: Ord item => (item -> item -> Ordering) -> item ->
                full -> full 
    insertBy cmp x ys
        | null ys = singleton x
        | otherwise = case cmp x (head ys) of
                        GT -> cons (head ys) (insertBy cmp x (tail ys))
                        _ ->  cons x (tail ys)

    {- | True if any items satisfy the function -}
    any :: (item -> Bool) -> full -> Bool
    any p = getAny . F.foldMap (Any . p)

    {- | True if the item occurs in the list -}
    elem :: Eq item => item -> full -> Bool
    elem i = any (== i)

    {- | Returns the index of the element, if it exists. -}
    elemIndex :: Eq item => item -> full -> Maybe Int
    elemIndex e l = findIndex (== e) l

    {- | Take a function and return the first matching element, or Nothing
       if there is no such element. -}
    find :: (item -> Bool) -> full -> Maybe item
    find f l = case findIndex f l of
                    Nothing -> Nothing
                    Just x -> Just (index l x)

    {- | Take a function and return the index of the first matching element,
         or Nothing if no element matches -}
    findIndex :: (item -> Bool) -> full -> Maybe Int
    findIndex f l = worker l 0
        where worker l' accum 
                | null l' = Nothing
                | f (head l') = Just accum
                | otherwise = worker (tail l') (accum + 1)

    {- | The element at 0-based index i.  Raises an exception if i is out
         of bounds.  Like (!!) for lists. -}
    index :: full -> Int -> item
    index l i 
        | null l = error "index: index not found"
        | i < 0 = error "index: index must be >= 0"
        | i == 0 = head l
        | otherwise = index (tail l) (i - 1)

    {- | Returns only the elements that satisfy the function. -}
    filter :: (item -> Bool) -> full -> full 
    filter func l 
        | null l = empty
        | func (head l) = cons (head l) (filter func (tail l))
        | otherwise = filter func (tail l)

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

    {- | Flatten the structure. -}
    concat :: (ListLike full' full, Monoid full) => full' -> full
    concat = F.fold

    {- | Map a function over the items and concatenate the results. -}
    concatMap :: (ListLike full' item') =>
                 (item -> full') -> full -> full'
    concatMap = F.foldMap

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

instance ListLike BS.ByteString Word8 where
    empty = BS.empty
    singleton = BS.singleton
    append = BS.append
    head = BS.head
    tail = BS.tail
    rigidMap = BS.map

{-
lookup k l
    | null l = fail "ListLike lookup: Key not found"
    | otherwise = case head l of
                    (k', v) -> if k' == k
                                   then return v
                                   else lookup k (tail l)
-}

data ListT k v = forall full. (Eq k, ListLike full (k, v)) => ListT full
withListT :: forall b k v. ListT k v -> (forall a. ListLike a (k, v) => a -> b) -> b
withListT s f =
    case s of
         ListT l -> f l
--data MapT k v = (Ord k, Eq k) => MapT (Map.Map k v)
--data forall full. ListLike full (k, v) => 
--    ListT k v = ListT full

class EncapIt a tp b c where
    mkEncap :: a -> tp b c

instance (Eq k, ListLike full (k, v)) => EncapIt full ListT k v where
    mkEncap = ListT

instance Ord k => EncapIt (Map.Map k v) Map.Map k v where
    mkEncap = id 
class MapLike ml where
    lookup :: (Eq k, Ord k, Monad m) => k -> ml k v -> m v

instance MapLike ListT where
    lookup k l' = withListT l' (worker k)
        where worker k l
                | null l = fail "ListLike lookup: Key not found"
                | otherwise = case head l of
                                (k', v) -> if k' == k
                                              then return v
                                              else worker k (tail l)

instance MapLike Map.Map where
    lookup = Map.lookup
{-

{-
instance (Ord x) => MapLike (Map.Map x v) where
    lookup = Map.lookup
-}

{-
instance (ListLike full (k, v), Eq k) => MapLike full where
    lookup k l 
        | null l = fail "ListLike lookup: Key not found"
        | otherwise = case head l of
                        (k', v) -> if k' == k
                                      then return v
                                      else lookup k (tail l)
-}
-}
testlist = [(1, "one"), (2, "two"), (3, "three")]
testmap = Map.fromList testlist
