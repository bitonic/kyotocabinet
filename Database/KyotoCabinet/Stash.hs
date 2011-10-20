module Database.KyotoCabinet.Stash
       ( Stash
       , StashOptions (..)
       , makeStash
       , module Database.KyotoCabinet.Operations
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

toTuningOptions :: StashOptions -> [TuningOption]
toTuningOptions = maybe [] ((: []) . Buckets) . buckets

className :: String
className = ":"

makeStash :: LoggingOptions -> StashOptions -> Mode -> IO Stash
makeStash log opts mode = makeVolatile Stash toTuningOptions className log opts mode
