{-# INCLUDE <kclangc.h> #-}
{-# LINE 1 "Database/KyotoCabinet/Foreign.hsc" #-}
{-# Language ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}
{-# LINE 2 "Database/KyotoCabinet/Foreign.hsc" #-}
module Database.KyotoCabinet.Foreign
       ( KCDB
         -- * Opening/creating/closing
       , kcdbnew
       , kcdbopen
       , kcdbclose
         -- ** Open modes
       , OpenMode
       , _KCOREADER
       , _KCOWRITER
       , _KCOCREATE
       , _KCOTRUNCATE
       , _KCOAUTOTRAN
       , _KCOAUTOSYNC
       , _KCONOLOCK
       , _KCOTRYLOCK
       , _KCONOREPAIR
         
         -- * Operations
       , kcdbset
       , kcdbget
         
         -- * Exceptions
       , KCException (..)
       , KCError (..)
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data (Typeable)
import Data.Int (Int32)
import Foreign.C.String (CString, newCAString, peekCAString) -- TODO: find out how to handle UTF8 names
import Foreign.C.Types (CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)



{-# LINE 43 "Database/KyotoCabinet/Foreign.hsc" #-}

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

-------------------------------------------------------------------------------                     

type OpenMode = Int32

_KCOREADER  :: OpenMode
_KCOREADER  =  1
_KCOWRITER  :: OpenMode
_KCOWRITER  =  2
_KCOCREATE  :: OpenMode
_KCOCREATE  =  4
_KCOTRUNCATE  :: OpenMode
_KCOTRUNCATE  =  8
_KCOAUTOTRAN  :: OpenMode
_KCOAUTOTRAN  =  16
_KCOAUTOSYNC  :: OpenMode
_KCOAUTOSYNC  =  32
_KCONOLOCK  :: OpenMode
_KCONOLOCK  =  64
_KCOTRYLOCK  :: OpenMode
_KCOTRYLOCK  =  128
_KCONOREPAIR  :: OpenMode
_KCONOREPAIR  =  256

{-# LINE 62 "Database/KyotoCabinet/Foreign.hsc" #-}

data KCDB

foreign import ccall "kclangc.h kcdbnew"
  kcdbnew :: IO (Ptr KCDB)

kcdbopen :: Ptr KCDB
            -> String  -- ^ File name
            -> Int32   -- ^ Open mode
            -> IO ()
kcdbopen db fn mode = newCAString fn >>= \fnptr -> kcdbopen' db fnptr mode >>= handleResult db "kcdbopen"
foreign import ccall "kclangc.h kcdbopen"
  kcdbopen' :: Ptr KCDB -> CString -> Int32 -> IO Int32

kcdbclose :: Ptr KCDB -> IO ()
kcdbclose db = kcdbclose' db >>= handleResult db "kcdbclose"
foreign import ccall "kclangc.h kcdbclose"
  kcdbclose' :: Ptr KCDB -> IO Int32

-------------------------------------------------------------------------------

kcdbset :: Ptr KCDB -> ByteString -> ByteString -> IO ()
kcdbset db k v = BS.useAsCStringLen k $ \(kptr, klen) ->
                 BS.useAsCStringLen v $ \(vptr, vlen) ->
                 kcdbset' db kptr (fi klen) vptr (fi vlen) >>= handleResult db "kcdbset"
foreign import ccall "kclangc.h kcdbset"
  kcdbset' :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO Int32

kcdbget :: Ptr KCDB -> ByteString -> IO ByteString
kcdbget db k = BS.useAsCStringLen k $ \(kptr, klen) ->
               alloca $ \vlenptr ->
               do vptr <- kcdbget' db kptr (fi klen) vlenptr
                  vlen <- peek vlenptr
                  BS.packCStringLen (vptr, fi vlen)
foreign import ccall "kclangc.h kcdbget"
  kcdbget' :: Ptr KCDB -> CString -> CSize -> Ptr CSize -> IO CString

-------------------------------------------------------------------------------

foreign import ccall "kclangc.h kcdbecode"
  kcdbecode :: Ptr KCDB -> IO Int32

kcdbemsg :: Ptr KCDB -> IO String
kcdbemsg db = kcdbemsg' db >>= peekCAString
foreign import ccall "kclangc.h kcdbemsg"
  kcdbemsg' :: Ptr KCDB -> IO CString

-------------------------------------------------------------------------------

data KCException = KCException { excFunction :: String
                               , excError    :: KCError
                               , excMsg      :: String
                               }
                   deriving (Show, Typeable)
instance Exception KCException

data KCError = Success | NotImplemented | InvalidOperation | NoRepository
             | NoPermission | BrokenFile | RecordDuplication | NoRecord
             | LogicalInconsistency | SystemError | MiscError
             deriving (Show, Typeable)

getError :: Int32 -> KCError
getError err | err == 0 = Success
{-# LINE 125 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 1  = NotImplemented
{-# LINE 126 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 2 = InvalidOperation
{-# LINE 127 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 3 = NoRepository
{-# LINE 128 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 4  = NoPermission
{-# LINE 129 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 5  = BrokenFile
{-# LINE 130 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 6  = RecordDuplication
{-# LINE 131 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 7   = NoRecord
{-# LINE 132 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 8   = LogicalInconsistency
{-# LINE 133 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 9  = SystemError
{-# LINE 134 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 15    = MiscError
{-# LINE 135 "Database/KyotoCabinet/Foreign.hsc" #-}
             | otherwise = error $ "Database.KyotoCabinet.Foreign: received unrecognised error n " ++ show err
                     

handleResult :: Ptr KCDB -> String -> Int32 -> IO ()
handleResult db fun status
  | status == 0 = throwIO =<< ((KCException fun) <$> fmap getError (kcdbecode db) <*> kcdbemsg db)
  | otherwise  = return ()
