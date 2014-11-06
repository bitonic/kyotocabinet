{-|
Stash database.

On-memory database, memory efficient.

* Persistence: /volatile/

* Algorithm: /red black tree/

* Complexity: /O(log n)/

* Sequence: /lexical order/

* Lock unit: /record (rwlock)/
-}
module Database.KyotoCabinet.DB.Stash
       ( Stash
       , StashOptions (..)
       , defaultStashOptions
       , makeStash
       )
       where

import Data.Int (Int64)

import Prelude hiding (log)

import Database.KyotoCabinet.Internal
import Database.KyotoCabinet.Operations

newtype Stash = Stash DB

instance WithDB Stash where
  getDB (Stash db) = db

data StashOptions = StashOptions { buckets :: Maybe Int64
                                 }
                  deriving (Show, Read, Eq, Ord)

defaultStashOptions :: StashOptions
defaultStashOptions = StashOptions { buckets = Nothing }

toTuningOptions :: StashOptions -> [TuningOption]
toTuningOptions = maybe [] ((: []) . Buckets) . buckets

className :: String
className = ":"

makeStash :: LoggingOptions -> StashOptions -> Mode -> IO Stash
makeStash log opts mode = makeVolatile Stash toTuningOptions className log opts mode
