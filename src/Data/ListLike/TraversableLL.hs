{-# LANGUAGE MultiParamTypeClasses
            ,FunctionalDependencies
            ,FlexibleInstances #-}

module Data.ListLike.TraversableLL
    (-- * FoldableLL Class
     TraversableLL(..),
     -- * Utilities
     rigidMapM
    ) where
import Prelude hiding (foldl, foldr, foldr1, sequence, sequence_, map, mapM, mapM_)
import Control.Applicative
import Control.Monad.Identity (Identity(..), runIdentity)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
import Data.ListLike.FoldableLL

{- | This is the primary class for structures that are to be considered
traversable. Minimal complete definition: 'rigidTraverse'.

These functions are used heavily in "Data.ListLike". -}
class (FoldableLL full item) =>
    TraversableLL full item | full -> item where

    {- | Like traverse, but without the possibility of changing the type of
       the item.  This can have performance benefits for things such as
       ByteStrings, since it will let the ByteString use its native
       low-level map implementation. -}
    rigidTraverse :: (Applicative f) => (item -> f item) -> full -> f full

    {- | Like map, but without the possibility of changing the type of
       the item.  This can have performance benefits for things such as
       ByteStrings, since it will let the ByteString use its native
       low-level map implementation. -}
    rigidMap :: (item -> item) -> full -> full
    rigidMap f = runIdentity . rigidTraverse (Identity . f)

instance TraversableLL [a] a where
    rigidTraverse = T.traverse
    rigidMap      = L.map
{-
instance (F.Traversable f) => TraversableLL (f a) a where
    ...
-}

{- | Map each element of a structure to a monadic action, evaluate
   these actions from left to right, and collect the results. -}
rigidMapM :: (TraversableLL full item, Monad m) => (item -> m item) -> full -> m full
rigidMapM f = unwrapMonad . rigidTraverse (WrapMonad . f)
