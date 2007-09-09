{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

{- |
   Module     : Data.ListLike.FoldableLL
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Generic tools for data structures that can be folded.

Written by John Goerzen, jgoerzen\@complete.org

-}
module Data.ListLike.FoldableLL 
    (-- * FoldableLL Class
     FoldableLL(..),
     -- * Utilities
     fold, foldMap
    ) where 
import Prelude hiding (foldl, foldr, foldr1)
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.ByteString as BS
import Data.Word
import Data.Maybe

{- | This is the primary class for structures that are to be considered 
foldable.  A minimum complete definition provides 'foldl' and 'foldr'.

Instances of 'FoldableLL' can be folded, and can be many and varied.

All instances of 'Data.Foldable.Foldable' are automatically instances
of 'FoldableLL'.  This includes things such as lists, trees, etc.
    
These functions are used heavily in "Data.ListLike". -}
class FoldableLL full item | full -> item where
    {- | Left-associative fold -}
    foldl :: (a -> item -> a) -> a -> full -> a

    {- | Strict version of 'foldl'. -}
    foldl' :: (a -> item -> a) -> a -> full -> a
    -- This implementation from Data.Foldable
    foldl' f a xs = foldr f' id xs a
        where f' x k z = k $! f z x

    -- | A variant of 'foldl' with no base case.  Requires at least 1
    -- list element.
    foldl1 :: (item -> item -> item) -> full -> item
    -- This implementation from Data.Foldable
    foldl1 f xs = fromMaybe (error "fold1: empty structure")
                    (foldl mf Nothing xs)
           where mf Nothing y = Just y
                 mf (Just x) y = Just (f x y)
    {- | Right-associative fold -}
    foldr :: (item -> b -> b) -> b -> full -> b

    -- | Strict version of 'foldr'
    foldr' :: (item -> b -> b) -> b -> full -> b
    -- This implementation from Data.Foldable
    foldr' f a xs = foldl f' id xs a
        where f' k x z = k $! f x z

    -- | Like 'foldr', but with no starting value
    foldr1 :: (item -> item -> item) -> full -> item
    -- This implementation from Data.Foldable
    foldr1 f xs = fromMaybe (error "foldr1: empty structure")
                    (foldr mf Nothing xs)
           where mf x Nothing = Just x
                 mf x (Just y) = Just (f x y)

{- | Combine the elements of a structure using a monoid.
     @'fold' = 'foldMap' id@ -}
fold :: (FoldableLL full item, Monoid item) => full -> item
fold = foldMap id

{- | Map each element to a monoid, then combine the results -}
foldMap :: (FoldableLL full item, Monoid m) => (item -> m) -> full -> m
foldMap f = foldr (mappend . f) mempty

instance (F.Foldable f) => FoldableLL (f a) a where
    foldl = F.foldl
    foldl1 = F.foldl1
    foldl' = F.foldl'
    foldr = F.foldr
    foldr1 = F.foldr1
    foldr' = F.foldr'

