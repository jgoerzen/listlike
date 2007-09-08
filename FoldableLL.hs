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
   Module     : FoldableLL
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : BSD

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with strings.

Written by John Goerzen, jgoerzen\@complete.org
-}

module FoldableLL where 
import Prelude hiding (foldl, foldr, foldr1)
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe

class FoldableLL full item | full -> item where
    foldl :: (a -> item -> a) -> a -> full -> a

    {- | Strict version of foldl. -}
    foldl' :: (a -> item -> a) -> a -> full -> a
    -- This implementation from Data.Foldable
    foldl' f z xs = foldr f' id xs z
        where f' x k z = k $! f z x

    -- | A variant of 'foldl' with no base case.  Requires at least 1
    -- list element.
    foldl1 :: (item -> item -> item) -> full -> item
    -- This implementation from Data.Foldable
    foldl1 f xs = fromMaybe (error "fold1: empty structure")
                    (foldl mf Nothing xs)
           where mf Nothing y = Just y
                 mf (Just x) y = Just (f x y)

    foldr :: (item -> b -> b) -> b -> full -> b

    -- | Strict version of 'foldr'
    foldr' :: (item -> b -> b) -> b -> full -> b
    -- This implementation from Data.Foldable
    foldr' f z xs = foldl f' id xs z
        where f' k x z = k $! f x z

    -- | Like 'foldr', but with no starting value
    foldr1 :: (item -> item -> item) -> full -> item
    -- This implementation from Data.Foldable
    foldr1 f xs = fromMaybe (error "foldr1: empty structure")
                    (foldr mf Nothing xs)
           where mf x Nothing = Just x
                 mf x (Just y) = Just (f x y)

fold :: (FoldableLL full item, Monoid item) => full -> item
fold = foldMap id

foldMap :: (FoldableLL full item, Monoid m) => (item -> m) -> full -> m
foldMap f = foldr (mappend . f) mempty

instance FoldableLL BS.ByteString Word8 where
    foldl = BS.foldl
    foldr = BS.foldr

instance (F.Foldable f) => FoldableLL (f a) a where
    foldl = F.foldl
    foldr = F.foldr

