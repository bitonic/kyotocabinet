module Database.KyotoCabinet
       ( -- * DB Classes
         DB
         -- ** Volatile classes
       , Volatile (..)
         -- ** Persistent classes
       , Persistent (..)

         -- * Operations
         -- ** Creation
       , newVolatile
       , openPersistent
         -- ** Closing
       , close
         -- ** Visitor
       , VisitorAction (..)
       , VisitorFull
       , VisitorEmpty
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
         -- ** Modifying
       , append
         -- ** Getting
       , get
       , getBulk
         -- ** Removing
       , remove
       , removeBulk
       , seize
       , clear
         -- ** Exporting/importing
       , copy
       , dump
       , load
         -- ** Info & stats
       , count
       , size
       , path
       , status
         -- ** Merging
       , MergeMode (..)
       , merge

         -- * Opening modes
       , Mode (..)
       , WriteMode (..)
       , ReadMode (..)

         -- * Exceptions
       , KCException (..)
       , KCError (..)

          -- * Logging
       , LoggingOptions (..)
       , LogFile (..)
       , LogKind (..)
       , defaultLoggingOptions

         -- * Tuning options
       , TuningOption (..)
       , Options (..)
       , Compressor (..)
       , Comparator (..)
       ) where

import Data.ByteString (ByteString)
import Data.Int (Int64, Int8)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)

import Prelude hiding (log, iterate)

import Database.KyotoCabinet.Foreign

newtype DB = DB {unDB :: ForeignPtr KCDB}

-------------------------------------------------------------------------------

formatName :: Class c => Maybe FilePath -> c -> LoggingOptions -> [TuningOption] -> String
formatName fn class' log opts = hashify $ maybeToList fn ++ [type'] ++ optss ++ logs
  where
    hashify = intercalate "#"

    eq k v = k ++ "=" ++ v

    type' = "type" `eq` className class'

    optss = map (uncurry eq . getKeyValue) opts

    logs = case log of
             LoggingOptions lfile lks lpx -> [ "log" `eq` (logFileStr lfile)
                                             , hashify (map (eq "logkinds" . logKindStr) lks)
                                             , "logpx" `eq` lpx
                                             ]

    logFileStr (File fp) = fp
    logFileStr StdOut    = "-"
    logFileStr StdErr    = "+"

    logKindStr Debug = "debug"
    logKindStr Info  = "info"
    logKindStr Warn  = "warn"
    logKindStr Error = "error"

newDB :: Ptr KCDB -> IO DB
newDB kcdb = fmap DB (mkFinalizer kcdbdel >>= flip newForeignPtr kcdb)

newVolatile :: Volatile
               -> LoggingOptions
               -> [TuningOption]
               -> Mode
               -> IO (DB)
newVolatile class' log opts mode =
  do kcdb <- kcdbnew
     kcdbopen kcdb (formatName Nothing class' log opts) mode
     newDB kcdb

openPersistent :: Persistent
                  -> FilePath
                  -> LoggingOptions
                  -> [TuningOption]
                  -> Mode
                  -> IO (DB)
openPersistent class' fn log opts mode =
  do kcdb <- kcdbnew
     kcdbopen kcdb (formatName (Just fn) class' log opts) mode
     newDB kcdb

-------------------------------------------------------------------------------

withDB0 :: (Ptr KCDB -> IO f) -> (DB -> IO f)
withDB0 act = flip withForeignPtr act . unDB

withDB1 :: (Ptr KCDB -> a -> IO f) -> (DB -> a -> IO f)
withDB1 act (DB db) a = withForeignPtr db $ \kcdb -> act kcdb a

withDB2 :: (Ptr KCDB -> a -> b -> IO f) ->
           (DB -> a -> b -> IO f)
withDB2 act (DB db) a b = withForeignPtr db $ \kcdb -> act kcdb a b

withDB4 :: (Ptr KCDB -> a -> b -> c -> d -> IO f) ->
           (DB -> a -> b -> c -> d -> IO f)
withDB4 act (DB db) a b c d = withForeignPtr db $ \kcdb -> act kcdb a b c d

-------------------------------------------------------------------------------

close :: DB -> IO ()
close = withDB0 kcdbclose

-------------------------------------------------------------------------------


type Writable = Bool

-- | Executes the 'VisitorFull' on the existent records, and 'VisitorEmpty' on the missing ones.
accept :: DB -> ByteString -> VisitorFull -> VisitorEmpty -> Writable -> IO ()
accept = withDB4 kcdbaccept

acceptBulk :: DB -> [ByteString] -> VisitorFull -> VisitorEmpty -> Writable -> IO ()
acceptBulk = withDB4 kcdbacceptbulk

iterate :: DB -> VisitorFull -> Writable -> IO ()
iterate = withDB2 kcdbiterate

scanPara :: DB -> VisitorFull -> Int -> IO ()
scanPara = withDB2 kcdbscanpara

-------------------------------------------------------------------------------

set :: DB -> ByteString -> ByteString -> IO ()
set = withDB2 kcdbset

setBulk :: DB -> [(ByteString, ByteString)] -> Bool -> IO Int64
setBulk = withDB2 kcdbsetbulk

add :: DB -> ByteString -> ByteString -> IO ()
add = withDB2 kcdbadd

replace :: DB -> ByteString -> ByteString -> IO ()
replace = withDB2 kcdbreplace

append :: DB -> ByteString -> ByteString -> IO ()
append = withDB2 kcdbappend

-------------------------------------------------------------------------------

get :: DB -> ByteString -> IO (Maybe ByteString)
get = withDB1 kcdbget

getBulk :: DB -> [ByteString] -> Bool -> IO [(ByteString, ByteString)]
getBulk = withDB2 kcdbgetbulk

-------------------------------------------------------------------------------

remove :: DB -> ByteString -> IO ()
remove = withDB1 kcdbremove

removeBulk :: DB -> [ByteString] -> Bool -> IO Int64
removeBulk = withDB2 kcdbremovebulk

seize :: DB -> ByteString -> IO (Maybe ByteString)
seize = withDB1 kcdbseize

clear :: DB -> IO ()
clear = withDB0 kcdbclear

-------------------------------------------------------------------------------

copy :: DB -> String -> IO ()
copy = withDB1 kcdbcopy

dump :: DB -> String -> IO ()
dump = withDB1 kcdbdumpsnap

load :: DB -> String -> IO ()
load = withDB1 kcdbloadsnap

-------------------------------------------------------------------------------

count :: DB -> IO Int64
count = withDB0 kcdbcount

size :: DB -> IO Int64
size = withDB0 kcdbsize

path :: DB -> IO String
path = withDB0 kcdbpath

status :: DB -> IO String
status = withDB0 kcdbstatus

-------------------------------------------------------------------------------

merge :: DB -> [DB] -> MergeMode -> IO ()
merge db dbs mode = go dbs [] $ \kcdbs -> withDB2 kcdbmerge db kcdbs mode
  where
    go []                kcdbs f = f $ reverse kcdbs
    go ((DB db') : dbs') kcdbs f = withForeignPtr db' $ \kcdb -> go dbs' (kcdb : kcdbs) f

-------------------------------------------------------------------------------

data LoggingOptions = LoggingOptions { logFile   :: LogFile
                                       -- ^ The file in which the log messages will be written
                                     , logKind   :: [LogKind]
                                     , logPrefix :: String
                                       -- ^ The prefix of each log message
                                     }

data LogFile = File FilePath | StdOut | StdErr

data LogKind = Debug | Info | Warn | Error

-- | Default logging options, outputting to stdout, all messages, and no prefix.
defaultLoggingOptions :: LoggingOptions
defaultLoggingOptions = LoggingOptions {logFile = StdOut, logKind = [Debug, Info, Warn, Error], logPrefix = ""}

-------------------------------------------------------------------------------

-- | A class of database
class Class c where
  -- | Returns the name as it should be for KyotoCabinet to understand it.
  className :: c -> String

-------------------------------------------------------------------------------

-- | A volatile - in RAM - database.
data Volatile = ProtoHash
                -- ^ Prototype hash database.
                --
                --   On-memory database implemented with @std::unordered_map@.
                --
                --     * Persistence: /volatile/
                --
                --     * Algorithm: /hash table/
                --
                --     * Complexity: /O(1)/
                --
                --     * Sequence: /undefined/
                --
                --     * Lock unit: /whole (rwlock)/
              | ProtoTree
                -- ^ Prototype tree database.
                --
                --  On-memory database implemented with @std::map@.
                --
                --     * Persistence: /volatile/
                --
                --     * Algorithm: /red black tree/
                --
                --     * Complexity: /O(log n)/
                --
                --     * Sequence: /lexical order/
                --
                --     * Lock unit: /whole (rwlock)/
              | Stash
                -- ^ Stash database.
                --
                --   On-memory database, memory efficient.
                --
                --     * Persistence: /volatile/
                --
                --     * Algorithm: /red black tree/
                --
                --     * Complexity: /O(log n)/
                --
                --     * Sequence: /lexical order/
                --
                --     * Lock unit: /record (rwlock)/
              | CacheHash
                -- ^ Cache hash databaseb.
                --
                --   On-memory database with definable size, and with a LRU algorithm for eviction.
                --
                --     * Persistence: /volatile/
                --
                --     * Algorithm: /hash table/
                --
                --     * Complexity: /O(1)/
                --
                --     * Sequence: /undefined/
                --
                --     * Lock unit: /record (mutex)/
              | CacheTree
                -- ^ Cache tree database
                --
                --   On-memory database using a B+ tree, useful for an ordered cache.
                --
                --     * Persistence: /volatile/
                --
                --     * Algorithm: /B+ tree/
                --
                --     * Complexity: /O(log n)/
                --
                --     * Sequence: /custom order/
                --
                --     * Lock unit: /page (mutex)/

instance Class Volatile where
  className ProtoHash = "-"
  className ProtoTree = "+"
  className Stash     = ":"
  className CacheHash = "*"
  className CacheTree = "%"

-------------------------------------------------------------------------------

-- | A persistent database
data Persistent = Hash
                  -- ^ File hash database
                  --
                  --   File hash database: tipical DBM.
                  --
                  --     * Persistence: /persistent/
                  --
                  --     * Algorithm: /hash table/
                  --
                  --     * Complexity: /O(1)/
                  --
                  --     * Sequence: /undefined/
                  --
                  --     * Lock unit: /record (rwlock)/
                | Tree
                  -- ^ File tree database
                  --
                  --   File database implemented with a B+: DBM with order.
                  --
                  --     * Persistence: /persistent/
                  --
                  --     * Algorithm: /B+ tree/
                  --
                  --     * Complexity: /O(log n)/
                  --
                  --     * Sequence: /custom order/
                  --
                  --     * Lock unit: /page (rwlock)/
                | Dir
                  -- ^ Directory hash database
                  --
                  --   Respective files stored in a directory of the file system.
                  --
                  --     * Persistence: /persistent/
                  --
                  --     * Algorithm: /undefined/
                  --
                  --     * Complexity: /undefined/
                  --
                  --     * Sequence: /undefined/
                  --
                  --     * Lock unit: /record (rwlock)/
                | Forest
                  -- ^ Directory tree database
                  --
                  --   Directory database of B+ tree: huge DBM with order.
                  --
                  --     * Persistence: /persistent/
                  --
                  --     * Algorithm: /B+ tree/
                  --
                  --     * Complexity: /O(log n)/
                  --
                  --     * Sequence: /custom order/
                  --
                  --     * Lock unit: /page (rwlock)/
                | Text
                  -- ^ Plain text database
                  --
                  --   Plain text file handled as a database.
                  --
                  --     * Persistence: /persistent/
                  --
                  --     * Algorithm: /plain text/
                  --
                  --     * Complexity: /undefined/
                  --
                  --     * Sequence: /stored order/
                  --
                  --     * Lock unit: /record (rwlock)/

instance Class Persistent where
  className Hash   = "kch"
  className Tree   = "kct"
  className Dir    = "kcd"
  className Forest = "kcf"
  className Text   = "kcx"

-------------------------------------------------------------------------------

data TuningOption = Options Options
                    -- ^ General tuning options
                    --
                    --   Available on 'CacheHash', 'CacheTree', 'Dir', and 'Forest'.
                  | Buckets Int64
                    -- ^ Number of buckets in the hash table. The default number is about 1 million.
                    --
                    --   Available on 'Stash', 'CacheHash', 'CacheTree', 'Hash', and 'Tree'.
                  | Compressor Compressor
                    -- ^ Compression algorithm used. 'DEFLATE' is the default one.
                    --
                    --   Available on 'CacheHash', 'CacheTree', 'Hash', and 'Tree'.
                  | CipherKey String
                    -- ^ The compressor cipher key
                    --
                    --   Available on 'CacheHash', 'CacheTree', 'Hash', 'Tree'
                  | MaxRecords Int64
                    -- ^ Maximum number of records. By default there is no limit and no records are expired.
                    --
                    --   Available on 'CacheHash', 'CacheTree', and 'Tree'.
                  | MaxSize Int64
                    -- ^ Maximum size of the database. The default there is no limit.
                    --
                    --   Available on 'CacheHash'.
                  | PageSize Int64
                    -- ^ Maximum size of each page in the tree. The default is 8192, double the page size
                    --   on most systems.
                    --
                    --   Available on 'CacheTree', and 'Tree'.
                  | Comparator Comparator
                    -- ^ Comparator used in the tree. 'Lexical' by default.
                    --
                    --   Available on 'CacheTree', and 'Tree'.
                  | PageCacheSize Int64
                    -- ^ Size of the page cache. The default is 64MB.
                    --
                    --   Available on 'CacheTree', and 'Tree'.
                  | AlignmentPow Int8
                    -- ^ Power of the alignment of the record size. Default is 3 (records aligned to 8)
                    --
                    --   Available on 'Hash', and 'Tree'.
                  | FreePoolPow Int8
                    -- ^ Power of the capacity of the free block pool. Default is 10.
                    --
                    --   Available on 'Hash', and 'Tree'.
                  | MMapSize Int64
                    -- ^ The mmapping size. Default is 64 MB
                    --
                    --   Available on 'Hash', and 'Tree'.
                  | DefragInterval Int64
                    -- ^ The unit step of the defragmentation (e.g. number of updates necessary to trigger the
                    --   deframmentation). The default defragmentation is disabled, enable if there are a lot of
                    --   updates.
                    --
                    --   Available on 'Hash', and 'Tree'.

-- | General tuning options
data Options = Compress -- ^ Enable compression of the keys and the values

-- | Compression algorithm used. 'DEFLATE' is the default one.
data Compressor = Zlib    -- ^ The raw zlib compressor
                | DEFLATE -- ^ zlib deflate compressor
                | Gz      -- ^ zlib gzip compressor
                | LZO     -- ^ LZO compressor
                | LZMA    -- ^ LZMA compressor
                | Arc     -- ^ Arcfour cipher

-- | Comparator used in the tree. 'Lexical' by default.
data Comparator = Lexical | Decimal

getKeyValue :: TuningOption -> (String, String)
getKeyValue (Options Compress) = ("opts", "c")
getKeyValue (Buckets i)        = ("bnum", show i)
getKeyValue (Compressor compr) = case compr of
                                   Zlib    -> (k, "zlib")
                                   DEFLATE -> (k, "def")
                                   Gz      -> (k, "gz")
                                   LZO     -> (k, "lzo")
                                   LZMA    -> (k, "lzma")
                                   Arc     -> (k, "arc")
  where k = "zcomp"
getKeyValue (CipherKey s)      = ("zkey", s)
getKeyValue (MaxRecords i)     = ("capcount", show i)
getKeyValue (MaxSize i)        = ("capsize", show i)
getKeyValue (PageSize i)       = ("psize", show i)
getKeyValue (Comparator comp)  = case comp of
                                   Lexical -> ("lex", k)
                                   Decimal -> ("dec", k)
  where k = "rcomp"
getKeyValue (PageCacheSize i)  = ("pccap", show i)
getKeyValue (AlignmentPow i)   = ("apow", show i)
getKeyValue (FreePoolPow i)    = ("fpow", show i)
getKeyValue (MMapSize i)       = ("msiz", show i)
getKeyValue (DefragInterval i) = ("dfunit", show i)
