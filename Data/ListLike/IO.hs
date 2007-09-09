{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.ListLike.IO
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

String-like functions

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.ListLike.IO
    ( ListLikeIO(..)
    )
       where
import Prelude hiding (length, head, last, null, tail, map, filter, concat, 
                       any, lookup, init, all, foldl, foldr, foldl1, foldr1,
                       maximum, minimum, iterate, span, break, takeWhile,
                       dropWhile, reverse, zip, zipWith, sequence,
                       sequence_, mapM, mapM_, concatMap, and, or, sum,
                       product, repeat, replicate, cycle, take, drop,
                       splitAt, elem, notElem, unzip, lines, words,
                       unlines, unwords, putStr, getContents)
import qualified System.IO as IO
import qualified Data.List as L
import Data.ListLike.Base
import Data.ListLike.Utils
import Data.ListLike.FoldableLL
import qualified Control.Monad as M
import Data.Monoid
import Data.Word
import qualified Data.Map as Map
import Data.Maybe

{- | An extension to 'ListLike' for those data types that support
I\/O.  Minimal complete definition:

* hGetLine

* hGetContents

* hGet

* hGetNonBlocking

* hPutStr
-}
class (ListLike full item) => ListLikeIO full item | full -> item where
    hGetLine :: IO.Handle -> IO full

    hGetContents :: IO.Handle -> IO full

    hGet :: IO.Handle -> Int -> IO full

    hGetNonBlocking :: IO.Handle -> Int -> IO full

    hPutStr :: IO.Handle -> full -> IO ()

    hPutStrLn :: IO.Handle -> full -> IO ()
    hPutStrLn fp x =
        do hPutStr fp x
           IO.hPutStrLn fp ""

    getLine :: IO full
    getLine = hGetLine IO.stdin

    getContents :: IO full
    getContents = hGetContents IO.stdin

    putStr :: full -> IO ()
    putStr = hPutStr IO.stdout

    putStrLn :: full -> IO ()
    putStrLn = hPutStrLn IO.stdout

    interact :: (full -> full) -> IO ()
    interact func = 
        do c <- getContents
           putStr (func c)

    readFile :: FilePath -> IO full
    readFile fn =
        do fp <- IO.openFile fn IO.ReadMode
           hGetContents fp

    writeFile :: FilePath -> full -> IO ()
    writeFile fn x =
        do fp <- IO.openFile fn IO.WriteMode
           hPutStr fp x
           IO.hClose fp

    appendFile :: FilePath -> full -> IO ()
    appendFile fn x =
        do fp <- IO.openFile fn IO.AppendMode
           hPutStr fp x
           IO.hClose fp
