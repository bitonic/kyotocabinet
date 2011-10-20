module Database.KyotoCabinet.Tree
       ( Tree
       , TreeOptions (..)
       , Compressor (..)
       , Options (..)
       , Comparator (..)
       , makeTree
       , openTree
       , module Database.KyotoCabinet.Operations
       )
       where

import Data.Int (Int64, Int8)
import Data.Maybe (maybeToList)

import Prelude hiding (log)

import Database.KyotoCabinet.Internal
import Database.KyotoCabinet.Operations

newtype Tree = Tree DB

instance WithDB Tree where
  getDB (Tree db) = db

data TreeOptions = TreeOptions { alignmentPow :: Maybe Int8
                               , freePoolPow :: Maybe Int8
                               , options :: [Options]
                               , buckets :: Maybe Int64
                               , maxSize :: Maybe Int64
                               , defragInterval :: Maybe Int64
                               , compressor :: Maybe Compressor
                               , cipherKey :: Maybe String
                               , pageSize :: Maybe Int64
                               , comparator :: Maybe Comparator
                               , pageCacheSize :: Maybe Int64
                               }
                 deriving (Show, Read, Eq, Ord)

toTuningOptions :: TreeOptions -> [TuningOption]
toTuningOptions TreeOptions { alignmentPow = ap
                            , freePoolPow = fp
                            , options = os
                            , buckets = bs
                            , maxSize = ms
                            , defragInterval = di
                            , compressor = cmp
                            , cipherKey = key
                            , pageSize = ps
                            , comparator = cmprtr
                            , pageCacheSize = pcs
                            } =
  mtl AlignmentPow ap ++ mtl FreePoolPow fp ++ map Options os ++ mtl Buckets bs ++
  mtl MaxSize ms ++ mtl DefragInterval di ++ mtl Compressor cmp ++ mtl CipherKey key ++
  mtl PageSize ps ++ mtl Comparator cmprtr ++ mtl PageCacheSize pcs
  where
    mtl f = maybeToList .  fmap f

className :: String
className = "kct"

makeTree :: FilePath -> LoggingOptions -> TreeOptions -> Mode -> IO Tree
makeTree fp log opts mode = makePersistent Tree toTuningOptions className fp log opts mode

openTree :: FilePath -> LoggingOptions -> Mode -> IO Tree
openTree fp log mode = openPersistent Tree className fp log mode