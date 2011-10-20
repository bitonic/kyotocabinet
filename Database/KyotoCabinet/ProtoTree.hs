module Database.KyotoCabinet.ProtoTree
       ( ProtoTree
       , makeProtoTree
       , module Database.KyotoCabinet.Operations
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
