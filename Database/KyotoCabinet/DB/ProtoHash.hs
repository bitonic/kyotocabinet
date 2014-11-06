{-|
Prototype hash database.

On-memory database implemented with @std::unordered_map@.

* Persistence: /volatile/

* Algorithm: /hash table/

* Complexity: /O(1)/

* Sequence: /undefined/

* Lock unit: /whole (rwlock)/
-}
module Database.KyotoCabinet.DB.ProtoHash
       ( ProtoHash
       , makeProtoHash
       )
       where

import Database.KyotoCabinet.Internal
import Database.KyotoCabinet.Operations

import Prelude hiding (log)

newtype ProtoHash = ProtoHash DB

instance WithDB ProtoHash where
  getDB (ProtoHash db) = db

className :: String
className = "-"

makeProtoHash :: LoggingOptions -> Mode -> IO ProtoHash
makeProtoHash log mode = makeVolatile ProtoHash id className log [] mode