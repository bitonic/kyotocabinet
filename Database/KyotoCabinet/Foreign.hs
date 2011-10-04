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
       , Mode (..)
       , ReadMode (..)
       , WriteMode (..)

         -- * Operations
       , kcdbset
       , kcdbget

         -- * Exceptions
       , KCException (..)
       , KCError (..)
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception, throwIO)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data (Typeable)
import Data.Int (Int32)
import Foreign.C.String (CString, newCAString, peekCAString) -- TODO: find out how to handle UTF8 names
import Foreign.C.Types (CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)


{-# LINE 36 "Database/KyotoCabinet/Foreign.hsc" #-}

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

-------------------------------------------------------------------------------

data Mode = Reader [ReadMode] | Writer [WriteMode] [ReadMode]

data WriteMode = Create | Truncate | AutoTran | AutoSync

data ReadMode = NoLock | TryLock | NoRepair

modeFlag :: Mode -> Int32
modeFlag (Reader ms)    = foldr (.|.) 1 $ map readFlag ms
{-# LINE 50 "Database/KyotoCabinet/Foreign.hsc" #-}
modeFlag (Writer ws rs) = foldr (.|.) (foldr (.|.) 1 $ map writeFlag ws) $
{-# LINE 51 "Database/KyotoCabinet/Foreign.hsc" #-}
                          map readFlag rs

readFlag :: ReadMode -> Int32
readFlag NoLock = 64
{-# LINE 55 "Database/KyotoCabinet/Foreign.hsc" #-}
readFlag TryLock = 128
{-# LINE 56 "Database/KyotoCabinet/Foreign.hsc" #-}
readFlag NoRepair = 256
{-# LINE 57 "Database/KyotoCabinet/Foreign.hsc" #-}

writeFlag :: WriteMode -> Int32
writeFlag Create   = 4
{-# LINE 60 "Database/KyotoCabinet/Foreign.hsc" #-}
writeFlag Truncate = 8
{-# LINE 61 "Database/KyotoCabinet/Foreign.hsc" #-}
writeFlag AutoTran = 16
{-# LINE 62 "Database/KyotoCabinet/Foreign.hsc" #-}
writeFlag AutoSync = 32
{-# LINE 63 "Database/KyotoCabinet/Foreign.hsc" #-}

-------------------------------------------------------------------------------

data KCDB

kcdbnew :: IO (Ptr KCDB)
kcdbnew =
  do kcdb <- kcdbnew'
     if kcdb == nullPtr then
       error "Database.KyotoCabinet.Foreign.kcdbnew: kcdbnew returned NULL."
       else return kcdb
foreign import ccall "kclangc.h kcdbnew"
  kcdbnew' :: IO (Ptr KCDB)

kcdbopen :: Ptr KCDB
            -> String -- ^ File name
            -> Mode   -- ^ Open mode
            -> IO ()
kcdbopen db fn mode = newCAString fn >>= \fnptr -> kcdbopen' db fnptr (modeFlag mode) >>=
                                                   handleResult db "kcdbopen"
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

kcdbget :: Ptr KCDB -> ByteString -> IO (Maybe ByteString)
kcdbget db k = BS.useAsCStringLen k $ \(kptr, klen) ->
               alloca $ \vlenptr ->
               do vptr <- kcdbget' db kptr (fi klen) vlenptr
                  if vptr == nullPtr then return Nothing
                    else peek vlenptr >>= \vlen -> fmap Just $ BS.packCStringLen (vptr, fi vlen)
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
{-# LINE 135 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 1  = NotImplemented
{-# LINE 136 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 2 = InvalidOperation
{-# LINE 137 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 3 = NoRepository
{-# LINE 138 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 4  = NoPermission
{-# LINE 139 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 5  = BrokenFile
{-# LINE 140 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 6  = RecordDuplication
{-# LINE 141 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 7   = NoRecord
{-# LINE 142 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 8   = LogicalInconsistency
{-# LINE 143 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 9  = SystemError
{-# LINE 144 "Database/KyotoCabinet/Foreign.hsc" #-}
             | err == 15    = MiscError
{-# LINE 145 "Database/KyotoCabinet/Foreign.hsc" #-}
             | otherwise = error $ "Database.KyotoCabinet.Foreign: received unrecognised error n " ++ show err

handleResult :: Ptr KCDB -> String -> Int32 -> IO ()
handleResult db fun status
  | status == 0 = throwIO =<< ((KCException fun) <$> fmap getError (kcdbecode db) <*> kcdbemsg db)
  | otherwise  = return ()
