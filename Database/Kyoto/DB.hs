{-# Language MultiParamTypeClasses #-}
module Database.Kyoto.DB
       ( -- * Logging
         LoggingOptions (..)
       , LogFile (..)
       , LogKind (..)
         
         -- * Tuning
       , TuningOption
         
         -- * Classes
       , Class
       , Volatile
       , Persistent
         -- ** Classes - options relashionship
       , HasOption
         -- ** Volatile classes
       , ProtoHash (..)
       , ProtoTree (..)
       , Stash (..)
       , CacheHash (..)
       , CacheTree (..)
         -- ** Persistent classes
       , Hash (..)
       , Tree (..)
       , Dir (..)
       , Forest (..)
       , Text (..)

         -- * The main DB type
       , DB
         
         -- * Opening modes
       , Mode (..)
       , WriteMode (..)
       , ReadMode (..)
         
         -- * Creation
       , newVolatile
       , openPersistent
         
         -- * Closing
       , close
         
         -- * Tuning
       , tune
         
         -- * Setting
       , set
         
         -- * Getting
       , get
       ) where

import Data.Serialize (Serialize)
import Foreign.ForeignPtr (ForeignPtr)

-------------------------------------------------------------------------------

data LoggingOptions = LoggingOptions { logFile   :: LogFile
                                     , logKind   :: LogKind
                                     , logPrefix :: String
                                     }

data LogFile = File FilePath | StdOut | StdErr

data LogKind = Debug | Info | Warn | Error

-------------------------------------------------------------------------------

class Class c where
  className :: c -> String

class Class c => Volatile c

class Class c => Persistent c

-------------------------------------------------------------------------------

data ProtoHash = ProtoHash
instance Class ProtoHash where className _ = "-"
instance Volatile ProtoHash

data ProtoTree = ProtoTree
instance Class ProtoTree where className _ = "+"
instance Volatile ProtoTree

data Stash = Stash
instance Class Stash where className _ = ":"
instance Volatile Stash

data CacheHash = CacheHash
instance Class CacheHash where className _ = "*"
instance Volatile CacheHash

data CacheTree = CacheTree
instance Class CacheTree where className _ = "%"
instance Volatile CacheTree

-------------------------------------------------------------------------------

data Hash = Hash
instance Class Hash where className _ = "kch"
instance Persistent Hash

data Tree = Tree
instance Class Tree where className _ = "kct"
instance Persistent Tree

data Dir = Dir
instance Class Dir where className _ = "kcd"
instance Persistent Dir

data Forest = Forest
instance Class Forest where className _ = "kcf"
instance Persistent Forest

data Text = Text
instance Class Text where className _ = "kcx"
instance Persistent Text
         
-------------------------------------------------------------------------------

class TuningOption o where
  keyValue :: Show v => o -> (String, v)

-------------------------------------------------------------------------------

class (Class c, TuningOption o) => HasOption c o

-------------------------------------------------------------------------------

newtype DB c key value = DB (ForeignPtr ())

-------------------------------------------------------------------------------

data Mode = Reader [ReadMode] | Writer [WriteMode] [ReadMode]

data WriteMode = Create | Truncate | AutoTran | AutoSinc

data ReadMode = NoLock | TryLock | NoRepair

                         
newVolatile :: (Serialize k, Serialize v, Volatile c)
               => c -> LoggingOptions -> Mode -> IO (DB c k v)
newVolatile = undefined

openPersistent :: (Serialize k, Serialize v, Persistent c)
                  => c -> FilePath -> LoggingOptions -> Mode -> IO (DB c k v)
openPersistent = undefined

-------------------------------------------------------------------------------

close :: DB c k v -> IO ()
close = undefined

-------------------------------------------------------------------------------

tune :: HasOption c o => DB c k v -> o -> IO ()
tune = undefined

-------------------------------------------------------------------------------

set :: (Serialize k, Serialize v) => DB c k v -> k -> v -> IO ()
set = undefined

-------------------------------------------------------------------------------

get :: (Serialize k, Serialize v) => DB c k v -> k -> IO (Maybe v)
get = undefined
