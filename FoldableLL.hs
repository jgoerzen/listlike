{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

{- |
   Module     : FoldableLL
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : LGPL

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

