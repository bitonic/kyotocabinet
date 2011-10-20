module Database.KyotoCabinet.CacheHash
       ( CacheHash
       , CacheHashOptions (..)
       , Compressor (..)
       , Options (..)
       , makeCacheHash
       , module Database.KyotoCabinet.Operations
       )
       where

import Data.Int (Int64)
import Data.Maybe (maybeToList)

import Prelude hiding (log)

import Database.KyotoCabinet.Internal
import Database.KyotoCabinet.Operations

newtype CacheHash = CacheHash DB

instance WithDB CacheHash where
  getDB (CacheHash db) = db

data CacheHashOptions = CacheHashOptions { options :: [Options]
                                         , buckets :: Maybe Int64
                                         , compressor :: Maybe Compressor
                                         , maxRecords :: Maybe Int64
                                         , maxSize :: Maybe Int64
                                         , cipherKey :: Maybe String
                                         }
                  deriving (Show, Read, Eq, Ord)

toTuningOptions :: CacheHashOptions -> [TuningOption]
toTuningOptions CacheHashOptions { options = os
                                 , buckets = bs
                                 , compressor = cmp
                                 , maxRecords = mr
                                 , maxSize = ms
                                 , cipherKey = key
                                 } =
  map Options os ++ mtl Buckets bs ++ mtl Compressor cmp ++ mtl MaxRecords mr ++
  mtl MaxSize ms ++ mtl CipherKey key
  where
    mtl f = maybeToList .  fmap f

className :: String
className = "*"

makeCacheHash :: LoggingOptions -> CacheHashOptions -> Mode -> IO CacheHash
makeCacheHash log opts mode = makeVolatile CacheHash toTuningOptions className log opts mode
