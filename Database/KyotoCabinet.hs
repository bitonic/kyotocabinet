{-# Language MultiParamTypeClasses, ExistentialQuantification #-}
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
       , parScan
         -- ** Setting
       , set
       , add
       , replace
         -- ** Modifying
       , append
         -- ** Getting
       , get
         -- ** Removing
       , remove

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
       , Options (..)
       , Buckets (..)
       , Compressor (..)
       , CipherKey (..)
       , MaxRecords (..)
       , MaxSize (..)
       , PageSize (..)
       , Comparator (..)
       , PageCacheSize (..)
       , AlignmentPow (..)
       , FreePoolPow (..)
       , MMapSize (..)
       , DefragInterval (..)
         -- ** Class option
       , ClassOption (..)
       ) where

import Data.ByteString (ByteString)
import Data.Int (Int64, Int8)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Foreign.Ptr (Ptr)

import Prelude hiding (log, iterate)

import Database.KyotoCabinet.Foreign


newtype DB c = DB (Ptr KCDB)

-------------------------------------------------------------------------------

formatName :: Class c => Maybe FilePath -> c -> LoggingOptions -> [ClassOption c] -> String
formatName fn class' log opts = hashify $ maybeToList fn ++ [type'] ++ optss ++ logs
  where
    hashify = intercalate "#"

    eq k v = k ++ "=" ++ v

    type' = "type" `eq` className class'
    
    optss = map (\(ClassOption o) -> let (k, v) = keyValue o in k `eq` v) opts

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

-- | A general option of a class, only useful to build the options list for 'newVolatile' and
--   'openPersistent'.
data ClassOption c = forall o. HasOption c o => ClassOption o

newVolatile :: Volatile c
               => c
               -> LoggingOptions
               -> [ClassOption c]
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
                  -> [ClassOption c]
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

parScan :: DB c -> VisitorFull -> Int -> IO ()
parScan (DB kcdb) = kcdbscanpara kcdb 

-------------------------------------------------------------------------------

set :: DB c -> ByteString -> ByteString -> IO ()
set (DB kcdb) = kcdbset kcdb

add :: DB c -> ByteString -> ByteString -> IO ()
add (DB kcdb) = kcdbadd kcdb

replace :: DB c -> ByteString -> ByteString -> IO ()
replace (DB kcdb) = kcdbreplace kcdb

-------------------------------------------------------------------------------

append :: DB c -> ByteString -> ByteString -> IO ()
append (DB kcdb) = kcdbappend kcdb

-------------------------------------------------------------------------------

get :: DB c -> ByteString -> IO (Maybe ByteString)
get (DB kcdb) k = kcdbget kcdb k

-------------------------------------------------------------------------------

remove :: DB c -> ByteString -> IO ()
remove (DB kcdb) = kcdbremove kcdb

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

class TuningOption o where
  keyValue :: o -> (String, String)

-- | General tuning options
data Options = Compress -- ^ Enable compression of the keys and the values
instance TuningOption Options where
  keyValue Compress = ("opts", "c")

-- | Number of buckets in the hash table. The default number is about 1 million.
newtype Buckets = Buckets Int64
instance TuningOption Buckets where
  keyValue (Buckets s) = ("bnum", show s)

-- | Compression algorithm used. 'DEFLATE' is the default one.
data Compressor = Zlib    -- ^ The raw zlib compressor
                | DEFLATE -- ^ zlib deflate compressor
                | Gz      -- ^ zlib gzip compressor
                | LZO     -- ^ LZO compressor
                | LZMA    -- ^ LZMA compressor
                | Arc     -- ^ Arcfour cipher
instance TuningOption Compressor where
  keyValue o = case o of
                 Zlib    -> (k, "zlib")
                 DEFLATE -> (k, "def")
                 Gz      -> (k, "gz")
                 LZO     -> (k, "lzo")
                 LZMA    -> (k, "lzma")
                 Arc     -> (k, "arc")
    where k = "zcomp"

-- | The compressor cipher key.
newtype CipherKey = CipherKey String
instance TuningOption CipherKey where
  keyValue (CipherKey k) = ("zkey", k)

-- | Maximum number of records. The default there is no limit and no records are expired.
newtype MaxRecords = MaxRecords Int64
instance TuningOption MaxRecords where
  keyValue (MaxRecords i) = ("capcount", show i)

-- | Maximum size of the database. The default there is no limit.
newtype MaxSize = MaxSize Int64
instance TuningOption MaxSize where
  keyValue (MaxSize i) = ("capsize", show i)

-- | Maximum size of each page in the tree. The default is 8192, double the page size on most systems.
newtype PageSize = PageSize Int64
instance TuningOption PageSize where
  keyValue (PageSize i) = ("psize", show i)

-- | Comparator used in the tree. 'Lexical' by default.
data Comparator = Lexical | Decimal
instance TuningOption Comparator where
  keyValue o = case o of
                 Lexical -> ("lex", k)
                 Decimal -> ("dec", k)
    where k = "rcomp"

-- | Size of the page cache. The default is 64MB.
newtype PageCacheSize = PageCacheSize Int64
instance TuningOption PageCacheSize where
  keyValue (PageCacheSize i) = ("pccap", show i)

-- | Power of the alignment of the record size. Default is 3 (records aligned to 8)
newtype AlignmentPow = AlignmentPow Int8
instance TuningOption AlignmentPow where
  keyValue (AlignmentPow i) = ("apow", show i)

-- | Power of the capacity of the free block pool. Default is 10.
newtype FreePoolPow = FreePoolPow Int8
instance TuningOption FreePoolPow where
  keyValue (FreePoolPow i) = ("fpow", show i)

-- | The mmapping size. Default is 64MB.
newtype MMapSize = MMapSize Int64
instance TuningOption MMapSize where
  keyValue (MMapSize i) = ("msiz", show i)

-- | The unit step of the defragmentation (e.g. number of updates necessary to trigger the
--   deframmentation). The default defragmentation is disabled, enable if there are a lot of
--   updates.
newtype DefragInterval = DefragInterval Int64
instance TuningOption DefragInterval where
  keyValue (DefragInterval i) = ("dfunit", show i)

-------------------------------------------------------------------------------

class (Class c, TuningOption o) => HasOption c o

instance HasOption Stash Buckets

instance HasOption CacheHash Options
instance HasOption CacheHash Buckets
instance HasOption CacheHash Compressor
instance HasOption CacheHash CipherKey
instance HasOption CacheHash MaxRecords
instance HasOption CacheHash MaxSize

instance HasOption CacheTree Options
instance HasOption CacheTree Buckets
instance HasOption CacheTree Compressor
instance HasOption CacheTree CipherKey
instance HasOption CacheTree MaxRecords
instance HasOption CacheTree PageSize
instance HasOption CacheTree PageCacheSize
instance HasOption CacheTree Comparator

instance HasOption Hash AlignmentPow
instance HasOption Hash FreePoolPow
instance HasOption Hash Options
instance HasOption Hash Buckets
instance HasOption Hash MMapSize
instance HasOption Hash DefragInterval
instance HasOption Hash Compressor
instance HasOption Hash CipherKey

instance HasOption Tree AlignmentPow
instance HasOption Tree FreePoolPow
instance HasOption Tree Options
instance HasOption Tree Buckets
instance HasOption Tree MMapSize
instance HasOption Tree DefragInterval
instance HasOption Tree Compressor
instance HasOption Tree CipherKey
instance HasOption Tree PageSize
instance HasOption Tree PageCacheSize
instance HasOption Tree Comparator

instance HasOption Dir Options

instance HasOption Forest Options

