{-# Language ScopedTypeVariables #-}

module Database.KyotoCabinet.Prova where

import Data.Serialize
import Data.ByteString (ByteString)
import Database.KyotoCabinet.DB


main = do
  db :: DB CacheHash ByteString ByteString <- newVolatile CacheHash (LoggingOptions {logFile = StdOut, logKind = Debug, logPrefix = ""}) (Reader [])
  return ()

  