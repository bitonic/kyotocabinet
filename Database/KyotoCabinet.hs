{-# Language MultiParamTypeClasses #-}
module Database.KyotoCabinet
       ( -- * Operations
         DB
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
         -- ** Exporting/importing
       , copy
       , dump
       , load
         -- ** Info & stats
       , count
       , size
       , path
       , status
         
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

         -- * Classes
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
import Foreign.Ptr (Ptr)

import Prelude hiding (log, iterate)

import Database.KyotoCabinet.Foreign

newtype DB c = DB {unDB :: Ptr KCDB}

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

newVolatile :: Volatile c
               => c
               -> LoggingOptions
               -> [TuningOption]
               -> Mode
               -> IO (DB c)
newVolatile class' log opts mode =
  do kcdb <- kcdbnew
     kcdbopen kcdb (formatName Nothing class' log opts) mode
     return $ DB kcdb

openPersistent :: Persistent c
                  => c
                  -> FilePath
                  -> LoggingOptions
                  -> [TuningOption]
                  -> Mode
                  -> IO (DB c)
openPersistent class' fn log opts mode =
  do kcdb <- kcdbnew
     kcdbopen kcdb (formatName (Just fn) class' log opts) mode
     return $ DB kcdb

-------------------------------------------------------------------------------

close :: DB c -> IO ()
close (DB kcdb) = kcdbclose kcdb

-------------------------------------------------------------------------------

type Writable = Bool

-- | Executes the 'VisitorFull' on the existent records, and 'VisitorEmpty' on the missing ones.
accept :: DB c -> ByteString -> VisitorFull -> VisitorEmpty -> Writable -> IO ()
accept (DB kcdb) = kcdbaccept kcdb

acceptBulk :: DB c -> [ByteString] -> VisitorFull -> VisitorEmpty -> Writable -> IO ()
acceptBulk (DB kcdb) = kcdbacceptbulk kcdb

iterate :: DB c -> VisitorFull -> Writable -> IO ()
iterate (DB kcdb) = kcdbiterate kcdb

scanPara :: DB c -> VisitorFull -> Int -> IO ()
scanPara (DB kcdb) = kcdbscanpara kcdb 

-------------------------------------------------------------------------------

set :: DB c -> ByteString -> ByteString -> IO ()
set (DB kcdb) = kcdbset kcdb

setBulk :: DB c -> [(ByteString, ByteString)] -> Bool -> IO Int64
setBulk (DB kcdb) = kcdbsetbulk kcdb

add :: DB c -> ByteString -> ByteString -> IO ()
add (DB kcdb) = kcdbadd kcdb

replace :: DB c -> ByteString -> ByteString -> IO ()
replace (DB kcdb) = kcdbreplace kcdb

append :: DB c -> ByteString -> ByteString -> IO ()
append (DB kcdb) = kcdbappend kcdb

-------------------------------------------------------------------------------

get :: DB c -> ByteString -> IO (Maybe ByteString)
get (DB kcdb) k = kcdbget kcdb k

getBulk :: DB c -> [ByteString] -> Bool -> IO [(ByteString, ByteString)]
getBulk (DB kcdb) = kcdbgetbulk kcdb

-------------------------------------------------------------------------------

remove :: DB c -> ByteString -> IO ()
remove (DB kcdb) = kcdbremove kcdb

removeBulk :: DB c -> [ByteString] -> Bool -> IO Int64
removeBulk (DB kcdb) = kcdbremovebulk kcdb

seize :: DB c -> ByteString -> IO (Maybe ByteString)
seize (DB kcdb) = kcdbseize kcdb

-------------------------------------------------------------------------------

copy :: DB c -> String -> IO ()
copy (DB kcdb) = kcdbcopy kcdb

dump :: DB c -> String -> IO ()
dump (DB kcdb) = kcdbdumpsnap kcdb

load :: DB c -> String -> IO ()
load (DB kcdb) = kcdbloadsnap kcdb

-------------------------------------------------------------------------------

count :: DB c -> IO Int64
count (DB kcdb) = kcdbcount kcdb

size :: DB c -> IO Int64
size (DB kcdb) = kcdbsize kcdb

path :: DB c -> IO String
path (DB kcdb) = kcdbpath kcdb

status :: DB c -> IO String
status (DB kcdb) = kcdbstatus kcdb

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

-- | Marks a volatile class - in memory.
class Class c => Volatile c

-- | Marks a persistent class - on disk.
class Class c => Persistent c

-------------------------------------------------------------------------------

-- | Prototype hash database.
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
data ProtoHash = ProtoHash
instance Class ProtoHash where className _ = "-"
instance Volatile ProtoHash

-- | Prototype tree database.
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
data ProtoTree = ProtoTree
instance Class ProtoTree where className _ = "+"
instance Volatile ProtoTree

-- | Stash database.
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
data Stash = Stash
instance Class Stash where className _ = ":"
instance Volatile Stash

-- | Cache hash database.
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
data CacheHash = CacheHash
instance Class CacheHash where className _ = "*"
instance Volatile CacheHash

-- | Cache tree database
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
data CacheTree = CacheTree
instance Class CacheTree where className _ = "%"
instance Volatile CacheTree

-------------------------------------------------------------------------------

-- | File hash database
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
data Hash = Hash
instance Class Hash where className _ = "kch"
instance Persistent Hash

-- | File tree database
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
data Tree = Tree
instance Class Tree where className _ = "kct"
instance Persistent Tree

-- | Directory hash database
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
data Dir = Dir
instance Class Dir where className _ = "kcd"
instance Persistent Dir

-- | Directory tree database
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
data Forest = Forest
instance Class Forest where className _ = "kcf"
instance Persistent Forest

-- | Plain text database
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
data Text = Text
instance Class Text where className _ = "kcx"
instance Persistent Text
         
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
