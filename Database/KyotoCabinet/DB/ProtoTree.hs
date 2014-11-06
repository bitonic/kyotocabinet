{-|
Prototype tree database.

On-memory database implemented with @std::map@.

* Persistence: /volatile/

* Algorithm: /red black tree/

* Complexity: /O(log n)/

* Sequence: /lexical order/

* Lock unit: /whole (rwlock)/
-}
module Database.KyotoCabinet.DB.ProtoTree
       ( ProtoTree
       , makeProtoTree
       )
       where

import Database.KyotoCabinet.Internal
import Database.KyotoCabinet.Operations

import Prelude hiding (log)

newtype ProtoTree = ProtoTree DB

instance WithDB ProtoTree where
  getDB (ProtoTree db) = db

className :: String
className = "+"

makeProtoTree :: LoggingOptions -> Mode -> IO ProtoTree
makeProtoTree log mode = makeVolatile ProtoTree id className log [] mode
