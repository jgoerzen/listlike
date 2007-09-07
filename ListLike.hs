{- arch-tag: String utilities main file
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
import Prelude hiding (length, head, last, null, tail, map, filter)
import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Control.Monad as M
import Data.Monoid
import Data.Traversable as T

{- | The class implementing list-like functions.

Implementators must define at least:

* empty
* singleton
* head
* tail
* append
* null or genericLength

-}
class (F.Foldable full, T.Traversable full) => ListLike full where
    {- | The empty list -}
    empty :: full item

    {- | Creates a single-itement list out of an itement -}
    singleton :: item -> full item

    {- | Like (:) for lists: adds an itement to the beginning of a list -}
    cons :: item -> full item -> full item
    cons item l = append (singleton item) l

    {- | Adds an itement to the *end* of a 'ListLike'. -}
    snoc :: full item -> item -> full item
    snoc l item = append l (singleton item)

    {- | Combines two lists.  Like (++). -}
    append :: full item -> full item -> full item

    {- | Extracts the first itement of a 'ListLike'. -}
    head :: full item -> item

    {- | Extracts the last itement of a 'ListLike'. -}
    last :: full item -> item
    last l = case genericLength l of
                  0 -> error "Called last on empty item"
                  1 -> head l
                  _ -> last (tail l)

    {- | Gives all itements after the head. -}
    tail :: full item -> full item

    {- | Tests whether the list is empty. -}
    null :: full item -> Bool
    null x = genericLength x == 0

    {- | Length of the list. -}
    length :: full item -> Int
    length = genericLength

    {- | Length of the list -}
    genericLength :: Num a => full item -> a
    genericLength l = calclen 0 l
        where calclen accum cl =
                  if null cl
                     then accum
                     else calclen (accum + 1) (tail cl)

    {- | Apply a function to each itement. -}
    map :: (item -> item) -> full item -> full item
    map = M.fmap

    {- | Reverse the itements in a list. -}
    reverse :: full item -> full item
    reverse l = rev l empty
        where rev rl a
                | null rl = a
                | otherwise = rev (tail rl) (cons (head rl) a)

    {- | Sorts the list. -}
    sort :: Ord item => full item -> full item
    sort = sortBy compare

    {- | Sort function taking a custom comparison function -}
    sortBy :: Ord item => (item -> item -> Ordering) -> full item -> full item
    sortBy cmp = F.foldr (insertBy cmp) empty

    {- | Inserts the itement at the last place where it is still less than or
         equal to the next itement -}
    insert :: Ord item => item -> full item -> full item
    insert = insertBy compare

    {- | Like 'insert', but with a custom comparison function -}
    insertBy :: Ord item => (item -> item -> Ordering) -> item ->
                full item -> full item
    insertBy cmp x ys
        | null ys = singleton x
        | otherwise = case cmp x (head ys) of
                        GT -> cons (head ys) (insertBy cmp x (tail ys))
                        _ ->  cons x (tail ys)

    {- | True if the item occurs in the list -}
    elem :: Eq item => item -> full item -> Bool
    elem = F.elem

    {- | Returns the index of the element, if it exists. -}
    elemIndex :: Eq item => item -> full item -> Maybe Int
    elemIndex e l = findIndex (== e) l

    {- | Take a function and return the first matching element, or Nothing
       if there is no such element. -}
    find :: (item -> Bool) -> full item -> Maybe item
    find = F.find

    {- | Take a function and return the index of the first matching element,
         or Nothing if no element matches -}
    findIndex :: (item -> Bool) -> full item -> Maybe Int
    findIndex f l = worker l 0
        where worker l' accum 
                | null l' = Nothing
                | f (head l') = Just accum
                | otherwise = worker (tail l') (accum + 1)

    {- | The element at 0-based index i.  Raises an exception if i is out
         of bounds.  Like (!!) for lists. -}
    index :: full item -> Int -> item
    index l i 
        | null l = error "index: index not found"
        | i < 0 = error "index: index must be >= 0"
        | i == 0 = head l
        | otherwise = index (tail l) (i - 1)

    {- | Returns only the elements that satisfy the function. -}
    filter :: (item -> Bool) -> full item -> full item
    filter func l 
        | null l = empty
        | func (head l) = cons (head l) (filter func (tail l))
        | otherwise = filter func (tail l)

    {- | Converts the structure to a list. -}
    toList :: full item -> [item]
    toList = F.toList

    {- | Generates the structure from a list. -}
    fromList :: [item] -> full item
    fromList [] = empty
    fromList (x:xs) = cons x (fromList xs)

instance ListLike [] where
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
    map = L.map
    reverse = L.reverse






