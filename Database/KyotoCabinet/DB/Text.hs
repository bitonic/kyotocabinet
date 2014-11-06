{-|
Plain text database.

Plain text file handled as a database.

* Persistence: /persistent/

* Algorithm: /plain text/

* Complexity: /undefined/

* Sequence: /stored order/

* Lock unit: /record (rwlock)/
-}
module Database.KyotoCabinet.DB.Text
       ( Text
       , makeText
       , openText
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

