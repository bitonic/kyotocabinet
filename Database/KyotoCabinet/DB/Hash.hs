{-|
File hash database.

File hash database: tipical DBM.

* Persistence: /persistent/

* Algorithm: /hash table/

* Complexity: /O(1)/

* Sequence: /undefined/

* Lock unit: /record (rwlock)/
-}
module Database.KyotoCabinet.DB.Hash
       ( Hash
       , HashOptions (..)
       , defaultHashOptions
       , Compressor (..)
       , Options (..)
       , Comparator (..)
       , makeHash
       , openHash
       )
       where

import Data.Int (Int64, Int8)
import Data.Maybe (maybeToList)

import Prelude hiding (log)

import Database.KyotoCabinet.Internal
import Database.KyotoCabinet.Operations

newtype Hash = Hash DB

instance WithDB Hash where
  getDB (Hash db) = db

data HashOptions = HashOptions { alignmentPow :: Maybe Int8
                               , freePoolPow :: Maybe Int8
                               , options :: [Options]
                               , buckets :: Maybe Int64
                               , maxSize :: Maybe Int64
                               , defragInterval :: Maybe Int64
                               , compressor :: Maybe Compressor
                               , cipherKey :: Maybe String
                               }
                 deriving (Show, Read, Eq, Ord)

defaultHashOptions :: HashOptions
defaultHashOptions = HashOptions { alignmentPow = Nothing
                                 , freePoolPow = Nothing
                                 , options = []
                                 , buckets = Nothing
                                 , maxSize = Nothing
                                 , defragInterval = Nothing
                                 , compressor = Nothing
                                 , cipherKey = Nothing
                                 }

toTuningOptions :: HashOptions -> [TuningOption]
toTuningOptions HashOptions { alignmentPow = ap
                            , freePoolPow = fp
                            , options = os
                            , buckets = bs
                            , maxSize = ms
                            , defragInterval = di
                            , compressor = cmp
                            , cipherKey = key
                            } =
  mtl AlignmentPow ap ++ mtl FreePoolPow fp ++ map Options os ++ mtl Buckets bs ++
  mtl MaxSize ms ++ mtl DefragInterval di ++ mtl Compressor cmp ++ mtl CipherKey key
  where
    mtl f = maybeToList .  fmap f

className :: String
className = "kch"

makeHash :: FilePath -> LoggingOptions -> HashOptions -> Mode -> IO Hash
makeHash fp log opts mode = makePersistent Hash toTuningOptions className fp log opts mode

openHash :: FilePath -> LoggingOptions -> Mode -> IO Hash
openHash fp log mode = openPersistent Hash className fp log mode
