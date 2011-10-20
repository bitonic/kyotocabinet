module Database.KyotoCabinet.Text
       ( Text
       , makeText
       , module Database.KyotoCabinet.Operations
       )
       where

import Database.KyotoCabinet.Internal
import Database.KyotoCabinet.Operations

import Prelude hiding (log)

newtype Text = Text DB

instance WithDB Text where
  getDB (Text db) = db

className :: String
className = "kcf"

makeText :: FilePath -> LoggingOptions -> Mode -> IO Text
makeText fp log mode = makePersistent Text id className fp log [] mode

openText :: FilePath -> LoggingOptions -> Mode -> IO Text
openText fp log mode = openPersistent Text className fp log mode

