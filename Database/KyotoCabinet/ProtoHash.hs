module Database.KyotoCabinet.ProtoHash
       ( ProtoHash
       , makeProtoHash
       , module Database.KyotoCabinet.Operations
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