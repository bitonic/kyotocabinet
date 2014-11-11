module Database.KyotoCabinet.Internal
       ( -- * DB Classes
         DB (..)
       , WithDB (..)

         -- Creation
       , makeVolatile
       , makePersistent
       , openPersistent

         -- * Logging options
       , LoggingOptions (..)
       , LogFile (..)
       , LogKind (..)
       , defaultLoggingOptions

         -- * Tuning options
       , TuningOption (..)
       , Options (..)
       , Compressor (..)
       , Comparator (..)
       , getKeyValue
         
         -- * Utils
       , formatName
       ) where

import Data.Int (Int64, Int8)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Ptr (Ptr)

import Prelude hiding (log)

import Database.KyotoCabinet.Foreign

-------------------------------------------------------------------------------

-- | Polymorphic database
newtype DB = DB {unDB :: ForeignPtr KCDB}

-- | A thing that has database
class WithDB db where
  -- | Access database
  getDB :: db -> DB

-------------------------------------------------------------------------------

newDB :: Ptr KCDB -> IO DB
newDB kcdb = fmap DB (newForeignPtr kcdbdel kcdb)

makeVolatile :: (DB -> a) -> (opts -> [TuningOption]) -> String
                -> LoggingOptions -> opts -> Mode -> IO a
makeVolatile constr optsconv class' log opts mode =
  do kcdb <- kcdbnew
     kcdbopen kcdb (formatName Nothing class' log (optsconv opts)) mode
     fmap constr $ newDB kcdb

makePersistent :: (DB -> a) -> (opts -> [TuningOption]) -> String
                -> FilePath -> LoggingOptions -> opts -> Mode -> IO a
makePersistent constr optsconv class' fn log opts mode =
  do kcdb <- kcdbnew
     kcdbopen kcdb (formatName (Just fn) class' log (optsconv opts)) mode
     fmap constr $ newDB kcdb

openPersistent :: (DB -> a) -> String
                -> FilePath -> LoggingOptions -> Mode -> IO a
openPersistent constr class' fn log mode =
  do kcdb <- kcdbnew
     kcdbopen kcdb (formatName (Just fn) class' log []) mode
     fmap constr $ newDB kcdb

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
defaultLoggingOptions = LoggingOptions { logFile = StdOut
                                       , logKind = [Debug, Info, Warn, Error]
                                       , logPrefix = ""
                                       }

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
                  deriving (Show, Read, Eq, Ord)

-- | General tuning options
data Options = Compress -- ^ Enable compression of the keys and the values
             deriving (Show, Read, Eq, Ord)

-- | Compression algorithm used. 'DEFLATE' is the default one.
data Compressor = Zlib    -- ^ The raw zlib compressor
                | DEFLATE -- ^ zlib deflate compressor
                | Gz      -- ^ zlib gzip compressor
                | LZO     -- ^ LZO compressor
                | LZMA    -- ^ LZMA compressor
                | Arc     -- ^ Arcfour cipher
                deriving (Show, Read, Eq, Ord)

-- | Comparator used in the tree. 'Lexical' by default.
data Comparator = Lexical | Decimal
                deriving (Show, Read, Eq, Ord)

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

-------------------------------------------------------------------------------

formatName :: Maybe FilePath
              -> String          -- ^ The DB type
              -> LoggingOptions
              -> [TuningOption]
              -> String
formatName fn class' log opts = hashify $ maybeToList fn ++ [type'] ++ optss ++ logs
  where
    hashify = intercalate "#"

    eq k v = k ++ "=" ++ v

    type' = "type" `eq` class'

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
