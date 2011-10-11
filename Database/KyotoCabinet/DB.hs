module Database.KyotoCabinet.DB
       ( -- * Logging options
         LoggingOptions (..)
       , LogFile (..)
       , LogKind (..)
       , defaultLoggingOptions

         -- * Opening mode
       , Mode (..)
       , WriteMode (..)
       , ReadMode (..)

         -- * Exceptions
       , KCException (..)
       , KCError (..)
       ) where

import Database.KyotoCabinet.Internal 
import Database.KyotoCabinet.Foreign

class WithDB db where
  getDB :: db -> DB




