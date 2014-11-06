{-|
Cache tree database

On-memory database using a B+ tree, useful for an ordered cache.

* Persistence: /volatile/

* Algorithm: /B+ tree/

* Complexity: /O(log n)/

* Sequence: /custom order/

* Lock unit: /page (mutex)/
-}
module Database.KyotoCabinet.DB.CacheTree
       ( CacheTree
       , CacheTreeOptions (..)
       , defaultCacheTreeOptions
       , Compressor (..)
       , Options (..)
       , Comparator (..)
       , makeCacheTree
       )
       where

import Data.Int (Int64)
import Data.Maybe (maybeToList)

import Prelude hiding (log)

import Database.KyotoCabinet.Internal
import Database.KyotoCabinet.Operations

newtype CacheTree = CacheTree DB

instance WithDB CacheTree where
  getDB (CacheTree db) = db

data CacheTreeOptions = CacheTreeOptions { options :: [Options]
                                         , buckets :: Maybe Int64
                                         , compressor :: Maybe Compressor
                                         , maxRecords :: Maybe Int64
                                         , cipherKey :: Maybe String
                                         , pageSize :: Maybe Int64
                                         , comparator :: Maybe Comparator
                                         , pageCacheSize :: Maybe Int64
                                         }
                  deriving (Show, Read, Eq, Ord)

defaultCacheTreeOptions :: CacheTreeOptions
defaultCacheTreeOptions = CacheTreeOptions { options = []
                                           , buckets = Nothing
                                           , compressor = Nothing
                                           , maxRecords = Nothing
                                           , cipherKey = Nothing
                                           , pageSize = Nothing
                                           , comparator = Nothing
                                           , pageCacheSize = Nothing
                                           }

toTuningOptions :: CacheTreeOptions -> [TuningOption]
toTuningOptions CacheTreeOptions { options = os
                                 , buckets = bs
                                 , compressor = cmp
                                 , maxRecords = mr
                                 , cipherKey = key
                                 , pageSize = ps
                                 , comparator = cmprt
                                 , pageCacheSize = pcs
                                 } =
  map Options os ++ mtl Buckets bs ++ mtl Compressor cmp ++ mtl MaxRecords mr ++
  mtl CipherKey key ++ mtl PageSize ps ++ mtl Comparator cmprt ++
  mtl PageCacheSize pcs
  where
    mtl f = maybeToList .  fmap f

className :: String
className = "%p"

makeCacheTree :: LoggingOptions -> CacheTreeOptions -> Mode -> IO CacheTree
makeCacheTree log opts mode = makeVolatile CacheTree toTuningOptions className log opts mode
