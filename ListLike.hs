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
import Prelude hiding (length, head, last, null, tail, map)
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Monoid

{- | The class implementing list-like functions.

Implementators must define at least:

* empty
* singleton
* head
* tail
* append
* null or length

-}
class (F.Foldable full) => ListLike full where
    {- | The empty list -}
    empty :: full elem

    {- | Creates a single-element list out of an element -}
    singleton :: elem -> full elem

    {- | Like (:) for lists: adds an element to the beginning of a list -}
    cons :: elem -> full elem -> full elem
    cons elem l = append (singleton elem) l

    {- | Adds an element to the *end* of a 'ListLike'. -}
    snoc :: full elem -> elem -> full elem
    snoc l elem = append l (singleton elem)

    {- | Combines two lists.  Like (++). -}
    append :: full elem -> full elem -> full elem

    {- | Extracts the first element of a 'ListLike'. -}
    head :: full elem -> elem

    {- | Extracts the last element of a 'ListLike'. -}
    last :: full elem -> elem
    last l = case length l of
                  0 -> error "Called last on empty item"
                  1 -> head l
                  x -> last (tail l)

    {- | Gives all elements after the head. -}
    tail :: full elem -> full elem

    {- | Tests whether the list is empty. -}
    null :: full elem -> Bool
    null x = length x == 0

    {- | Length of the list. -}
    length :: full elem -> Int
    length l = calclen 0 l
        where calclen accum cl =
                  if null cl
                     then accum
                     else calclen (accum + 1) (tail cl)

    {- | Apply a function to each element. -}
    map :: (elem -> elem) -> full elem -> full elem
    map f inp 
        | null inp = empty
        | otherwise = cons (f (head inp)) (map f (tail inp))

    {- | Reverse the elements in a list. -}
    reverse :: full elem -> full elem
    reverse l = rev l empty
        where rev rl a
                | null rl = a
                | otherwise = rev (tail rl) (cons (head rl) a)

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






