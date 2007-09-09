{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.ListLike.String
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

String-like functions

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.ListLike.String
    ( StringLike(..)
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
import Data.ListLike.Utils
import Data.ListLike.FoldableLL
import qualified Control.Monad as M
import Data.Monoid
import Data.Word
import qualified Data.Map as Map
import Data.Maybe

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
