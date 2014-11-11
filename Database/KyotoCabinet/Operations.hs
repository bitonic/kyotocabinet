{-# Language ExistentialQuantification #-}
module Database.KyotoCabinet.Operations
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

         -- * Type class
       , WithDB(..)
       , DB

         -- * Operations
         -- ** Closing
       , close
         -- ** Visitor
       , VisitorFull
       , VisitorEmpty
       , VisitorAction (..)
       , Writable
       , accept
       , acceptBulk
       , iterate
       , scanPara
         -- ** Setting
       , set
       , setBulk
       , add
       , replace
       , append
         -- ** Getting
       , get
       , getBulk
         -- ** Remove
       , remove
       , removeBulk
       , seize
       , clear
         -- ** Copying/Backup
       , copy
       , dump
       , load
         -- ** Info and statistics
       , count
       , size
       , path
       , status
         -- ** Merging
       , MergeMode (..)
       , GenericDB (..)
       , merge

         -- * Cursors
       , Cursor
         -- ** Creation
       , cursor
         -- ** Visitor
       , curAccept
         -- ** Setting
       , curSetValue
         -- ** Getting
       , curGetKey
       , curGetValue
       , curGet
         -- ** Removing
       , curRemove
       , curSeize
         -- ** Moving
       , curJump
       , curJumpKey
       , curJumpBack
       , curJumpBackKey
       , curStep
       , curStepBack
       ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)

import Prelude hiding (iterate, log)

import Database.KyotoCabinet.Internal 
import Database.KyotoCabinet.Foreign

-------------------------------------------------------------------------------

withFor0 :: (h -> ForeignPtr c) -> (Ptr c -> IO f) -> (h -> IO f)
withFor0 un act = flip withForeignPtr act . un

withFor1 :: (h -> ForeignPtr c) -> (Ptr c -> a -> IO f) -> (h -> a -> IO f)
withFor1 un act h a = withForeignPtr (un h) $ \kcdb -> act kcdb a

withFor2 :: (h -> ForeignPtr c) -> (Ptr c -> a -> b -> IO f)
            -> (h -> a -> b -> IO f)
withFor2 un act h a b = withForeignPtr (un h) $ \kcdb -> act kcdb a b

withFor3 :: (h -> ForeignPtr c) -> (Ptr c -> a -> b -> d -> IO f)
            -> (h -> a -> b -> d -> IO f)
withFor3 un act h a b c = withForeignPtr (un h) $ \kcdb -> act kcdb a b c

withFor4 :: (h -> ForeignPtr c) -> (Ptr c -> a -> b -> d -> e -> IO f)
            -> (h -> a -> b -> d -> e -> IO f)
withFor4 un act h a b c d = withForeignPtr (un h) $ \kcdb -> act kcdb a b c d

-------------------------------------------------------------------------------

close :: WithDB db => db -> IO ()
close = withFor0 unDB kcdbclose . getDB

-------------------------------------------------------------------------------

type Writable = Bool

-- | Executes the 'VisitorFull' on the existent records, and 'VisitorEmpty' on the missing ones.
accept :: WithDB db => db -> ByteString -> VisitorFull -> VisitorEmpty -> Writable -> IO ()
accept = withFor4 (unDB . getDB) kcdbaccept

acceptBulk :: WithDB db => db -> [ByteString] -> VisitorFull -> VisitorEmpty -> Writable -> IO ()
acceptBulk = withFor4 (unDB . getDB) kcdbacceptbulk

iterate :: WithDB db => db -> VisitorFull -> Writable -> IO ()
iterate = withFor2 (unDB . getDB) kcdbiterate

scanPara :: WithDB db => db -> VisitorFull -> Int -> IO ()
scanPara = withFor2 (unDB . getDB) kcdbscanpara

-------------------------------------------------------------------------------

set :: WithDB db => db -> ByteString -> ByteString -> IO ()
set = withFor2 (unDB . getDB) kcdbset

setBulk :: WithDB db => db -> [(ByteString, ByteString)] -> Bool -> IO Int64
setBulk = withFor2 (unDB . getDB) kcdbsetbulk

add :: WithDB db => db -> ByteString -> ByteString -> IO ()
add = withFor2 (unDB . getDB) kcdbadd

replace :: WithDB db => db -> ByteString -> ByteString -> IO ()
replace = withFor2 (unDB . getDB) kcdbreplace

append :: WithDB db => db -> ByteString -> ByteString -> IO ()
append = withFor2 (unDB . getDB) kcdbappend

-------------------------------------------------------------------------------

get :: WithDB db => db -> ByteString -> IO (Maybe ByteString)
get = withFor1 (unDB . getDB) kcdbget

getBulk :: WithDB db => db -> [ByteString] -> Bool -> IO [(ByteString, ByteString)]
getBulk = withFor2 (unDB . getDB) kcdbgetbulk

-------------------------------------------------------------------------------

remove :: WithDB db => db -> ByteString -> IO ()
remove = withFor1 (unDB . getDB) kcdbremove

removeBulk :: WithDB db => db -> [ByteString] -> Bool -> IO Int64
removeBulk = withFor2 (unDB . getDB) kcdbremovebulk

seize :: WithDB db => db -> ByteString -> IO (Maybe ByteString)
seize = withFor1 (unDB . getDB) kcdbseize

clear :: WithDB db => db -> IO ()
clear = withFor0 (unDB . getDB) kcdbclear

-------------------------------------------------------------------------------

copy :: WithDB db => db -> FilePath -> IO ()
copy = withFor1 (unDB . getDB) kcdbcopy

dump :: WithDB db => db -> FilePath -> IO ()
dump = withFor1 (unDB . getDB) kcdbdumpsnap

load :: WithDB db => db -> FilePath -> IO ()
load = withFor1 (unDB . getDB) kcdbloadsnap

-------------------------------------------------------------------------------

count :: WithDB db => db -> IO Int64
count = withFor0 (unDB . getDB) kcdbcount

size :: WithDB db => db -> IO Int64
size = withFor0 (unDB . getDB) kcdbsize

path :: WithDB db => db -> IO String
path = withFor0 (unDB . getDB) kcdbpath

status :: WithDB db => db -> IO String
status = withFor0 (unDB . getDB) kcdbstatus

-------------------------------------------------------------------------------

data GenericDB = forall db. WithDB db => GenericDB db

merge :: WithDB db => db -> [GenericDB] -> MergeMode -> IO ()
merge db dbs mode = go dbs [] $ \kcdbs -> withFor2 (unDB . getDB) kcdbmerge db kcdbs mode
  where
    go [] kcdbs f = f $ reverse kcdbs
    go ((GenericDB db') : dbs') kcdbs f =
      withForeignPtr (unDB . getDB $ db') $ \kcdb -> go dbs' (kcdb : kcdbs) f

-------------------------------------------------------------------------------

newtype Cursor = Cursor {unCursor :: ForeignPtr KCCUR}

-------------------------------------------------------------------------------

cursor :: WithDB db => db -> IO Cursor
cursor db = fmap Cursor (withForeignPtr (unDB . getDB $ db) $
                         \kcdb -> kcdbcursor kcdb >>= newForeignPtr kccurdel)

-------------------------------------------------------------------------------

curAccept :: Cursor -> VisitorFull -> Bool -> Bool -> IO ()
curAccept = withFor3 unCursor kccuraccept

curSetValue :: Cursor -> ByteString -> Bool -> IO ()
curSetValue = withFor2 unCursor kccursetvalue

curRemove :: Cursor -> IO ()
curRemove = withFor0 unCursor kccurremove

curGetKey :: Cursor -> Bool -> IO ByteString
curGetKey = withFor1 unCursor kccurgetkey

curGetValue :: Cursor -> Bool -> IO ByteString
curGetValue = withFor1 unCursor kccurgetvalue

curGet :: Cursor -> Bool -> IO (ByteString, ByteString)
curGet = withFor1 unCursor kccurget

curSeize :: Cursor -> IO (ByteString, ByteString)
curSeize = withFor0 unCursor kccurseize

curJump :: Cursor -> IO ()
curJump = withFor0 unCursor kccurjump

curJumpKey :: Cursor -> ByteString -> IO ()
curJumpKey = withFor1 unCursor kccurjumpkey

curJumpBack :: Cursor -> IO ()
curJumpBack = withFor0 unCursor kccurjumpback

curJumpBackKey :: Cursor -> ByteString -> IO ()
curJumpBackKey = withFor1 unCursor kccurjumpbackkey

curStep :: Cursor -> IO ()
curStep = withFor0 unCursor kccurstep

curStepBack :: Cursor -> IO ()
curStepBack = withFor0 unCursor kccurstepback


