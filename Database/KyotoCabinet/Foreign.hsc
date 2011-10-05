{-# Language ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}
module Database.KyotoCabinet.Foreign
       ( KCDB
       , kcdbnew
       , kcdbopen
       , kcdbclose
       , Mode (..)
       , ReadMode (..)
       , WriteMode (..)
       , VisitorAction (..)
       , VisitorFull
       , VisitorEmpty
       , kcdbaccept
       , kcdbacceptbulk
       , kcdbiterate
       , kcdbscanpara
       , kcdbset
       , kcdbadd
       , kcdbreplace
       , kcdbappend
       , kcdbget
       , kcdbremove
       , KCException (..)
       , KCError (..)
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception, throwIO)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Data (Typeable)
import Data.Int (Int32)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CSize, CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.Storable (Storable (..))

#include <kclangc.h>

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

boolToInt :: Bool -> Int32
boolToInt True  = 1
boolToInt False = 0

-------------------------------------------------------------------------------

data Mode = Reader [ReadMode] | Writer [WriteMode] [ReadMode]

data WriteMode = Create | Truncate | AutoTran | AutoSync

data ReadMode = NoLock | TryLock | NoRepair

modeFlag :: Mode -> Int32
modeFlag (Reader ms)    = foldr (.|.) #{const KCOREADER} $ map readFlag ms
modeFlag (Writer ws rs) = foldr (.|.) (foldr (.|.) #{const KCOWRITER} $ map writeFlag ws) $
                          map readFlag rs

readFlag :: ReadMode -> Int32
readFlag NoLock   = #{const KCONOLOCK}
readFlag TryLock  = #{const KCOTRYLOCK}
readFlag NoRepair = #{const KCONOREPAIR}

writeFlag :: WriteMode -> Int32
writeFlag Create   = #{const KCOCREATE}
writeFlag Truncate = #{const KCOTRUNCATE}
writeFlag AutoTran = #{const KCOAUTOTRAN}
writeFlag AutoSync = #{const KCOAUTOSYNC}

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
kcdbopen db fn mode = withCString fn $ \fnptr -> kcdbopen' db fnptr (modeFlag mode) >>=
                                                 handleBoolResult db "kcdbopen"
foreign import ccall "kclangc.h kcdbopen"
  kcdbopen' :: Ptr KCDB -> CString -> Int32 -> IO Int32

kcdbclose :: Ptr KCDB -> IO ()
kcdbclose db = kcdbclose' db >>= handleBoolResult db "kcdbclose"
foreign import ccall "kclangc.h kcdbclose"
  kcdbclose' :: Ptr KCDB -> IO Int32

-------------------------------------------------------------------------------

data VisitorAction = NoOperation -- ^ Don't do anything
                   | Remove      -- ^ Remove the record

type VisitorFull = ByteString       -- ^ Key
                   -> ByteString    -- ^ Value
                   -> IO (Either VisitorAction ByteString)
                   -- ^ If a 'ByteString' is returned, the value is changed.

type VisitorEmpty = ByteString
                    -> IO (Maybe ByteString)
                    -- ^ If the 'ByteString' is present, the value will be added.

foreign import ccall unsafe "utils.h _KCVISNOP"
  _KCVISNOP :: IO CString

foreign import ccall unsafe "utils.h _KCVISREMOVE"
  _KCVISREMOVE :: IO CString

type KCVISITFULL = FunPtr (CString -> CSize -> CString -> CSize -> Ptr CSize -> Ptr () -> IO CString)

mkVisitorFull :: VisitorFull -> IO KCVISITFULL
mkVisitorFull visitor =
  mkKCVISITFULL $ \kptr klen vptr vlen sizeptr _opq ->
  do k <- BS.unsafePackCStringLen (kptr, fi klen)
     v <- BS.unsafePackCStringLen (vptr, fi vlen)
     res <- visitor k v
     case res of
       Left NoOperation -> _KCVISNOP
       Left Remove      -> _KCVISREMOVE
       Right newv -> BS.unsafeUseAsCStringLen newv $ \(newvptr, len) -> poke sizeptr (fi len) >> return newvptr
foreign import ccall "wrapper"  
  mkKCVISITFULL :: (CString -> CSize -> CString -> CSize -> Ptr CSize -> Ptr () -> IO CString)
                   -> IO KCVISITFULL

type KCVISITEMPTY = FunPtr (CString -> CSize -> Ptr CSize -> Ptr () -> IO CString)

mkVisitorEmpty :: VisitorEmpty -> IO KCVISITEMPTY
mkVisitorEmpty visitor =
  mkKCVISITEMPTY $ \kptr klen sizeptr _opq ->
  do k <- BS.unsafePackCStringLen (kptr, fi klen)
     res <- visitor k
     case res of
       Nothing -> _KCVISNOP
       Just v  -> BS.unsafeUseAsCStringLen v $ \(vptr, vlen) -> poke sizeptr (fi vlen) >> return vptr
foreign import ccall "wrapper"
  mkKCVISITEMPTY :: (CString -> CSize -> Ptr CSize -> Ptr () -> IO CString)
                    -> IO KCVISITEMPTY

kcdbaccept :: Ptr KCDB -> ByteString -> VisitorFull -> VisitorEmpty -> Bool -> IO ()
kcdbaccept db k vf ve w =
  BS.unsafeUseAsCStringLen k $ \(kptr, klen) ->
  do vfptr <- mkVisitorFull vf
     veptr <- mkVisitorEmpty ve
     kcdbaccept' db kptr (fi klen) vfptr veptr nullPtr (boolToInt w) >>= handleBoolResult db "kcdbaccept"
foreign import ccall "kclangc.h kcdbaccept"
  kcdbaccept' :: Ptr KCDB -> CString -> CSize -> KCVISITFULL -> KCVISITEMPTY -> Ptr () -> Int32 -> IO Int32

kcdbacceptbulk :: Ptr KCDB -> [ByteString] -> VisitorFull -> VisitorEmpty -> Bool -> IO ()
kcdbacceptbulk db ks vf ve w =
  go ks [] $ \kcstrs ->
  withArrayLen kcstrs $ \len kcstrptr ->
  do vfptr <- mkVisitorFull vf
     veptr <- mkVisitorEmpty ve
     kcdbacceptbulk' db kcstrptr (fi len) vfptr veptr nullPtr (boolToInt w) >>= handleBoolResult db "kcdbacceptbulk"
  where
    go []        kcstrs f = f $ reverse kcstrs
    go (k : ks') kcstrs f = withKCSTR k $ \kcstr -> go ks' (kcstr : kcstrs) f
foreign import ccall "kclangc.h kcdbacceptbulk" 
  kcdbacceptbulk' :: Ptr KCDB -> Ptr KCSTR -> CSize -> KCVISITFULL -> KCVISITEMPTY -> Ptr () -> Int32 -> IO Int32

kcdbiterate :: Ptr KCDB -> VisitorFull -> Bool -> IO ()
kcdbiterate db vf w =
  do vfptr <- mkVisitorFull vf
     kcdbiterate' db vfptr nullPtr (boolToInt w) >>= handleBoolResult db "kcdbiterate"
foreign import ccall "kclangc.h kcdbiterate"
  kcdbiterate' :: Ptr KCDB -> KCVISITFULL -> Ptr () -> Int32 -> IO Int32

kcdbscanpara :: Ptr KCDB -> VisitorFull -> Int -> IO ()
kcdbscanpara db vf threads =
  do vfptr <- mkVisitorFull vf
     kcdbscanpara' db vfptr nullPtr (fi threads) >>= handleBoolResult db "kcdbscanpara"
foreign import ccall "kclangc.h kcdbscanpara"
  kcdbscanpara' :: Ptr KCDB -> KCVISITFULL -> Ptr () -> CSize -> IO Int32

-------------------------------------------------------------------------------

keyValFun :: (Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO Int32) -> String
             -> (Ptr KCDB -> ByteString -> ByteString -> IO ())
keyValFun f fn db k v = BS.unsafeUseAsCStringLen k $ \(kptr, klen) ->
                        BS.unsafeUseAsCStringLen v $ \(vptr, vlen) ->
                        f db kptr (fi klen) vptr (fi vlen) >>= handleBoolResult db fn

keyFun :: (Ptr KCDB -> CString -> CSize -> IO Int32) -> String
          -> (Ptr KCDB -> ByteString -> IO ())
keyFun f fn db k = BS.unsafeUseAsCStringLen k $ \(kptr, klen) ->
                   f db kptr (fi klen) >>= handleBoolResult db fn

-------------------------------------------------------------------------------

kcdbset :: Ptr KCDB -> ByteString -> ByteString -> IO ()
kcdbset = keyValFun kcdbset' "kcdbset"
foreign import ccall "kclangc.h kcdbset"
  kcdbset' :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO Int32

kcdbadd :: Ptr KCDB -> ByteString -> ByteString -> IO ()
kcdbadd = keyValFun kcdbadd' "kcdbadd"
foreign import ccall "kclangc.h kcdbadd"
  kcdbadd' :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO Int32

kcdbreplace :: Ptr KCDB -> ByteString -> ByteString -> IO ()
kcdbreplace = keyValFun kcdbreplace' "kcdbreplace"
foreign import ccall "kclangc.h kcdbreplace"
  kcdbreplace' :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO Int32

-------------------------------------------------------------------------------

kcdbappend :: Ptr KCDB -> ByteString -> ByteString -> IO ()
kcdbappend = keyValFun kcdbappend' "kcdbappend"
foreign import ccall "kclangc.h kcdbappend"
  kcdbappend' :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO Int32

-------------------------------------------------------------------------------

kcdbget :: Ptr KCDB -> ByteString -> IO (Maybe ByteString)
kcdbget db k = BS.unsafeUseAsCStringLen k $ \(kptr, klen) ->
               alloca $ \vlenptr ->
               do vptr <- kcdbget' db kptr (fi klen) vlenptr
                  if vptr == nullPtr then return Nothing
                    else peek vlenptr >>= \vlen -> fmap Just $ BS.unsafePackCStringLen (vptr, fi vlen)
foreign import ccall "kclangc.h kcdbget"
  kcdbget' :: Ptr KCDB -> CString -> CSize -> Ptr CSize -> IO CString

-------------------------------------------------------------------------------

kcdbremove :: Ptr KCDB -> ByteString -> IO ()
kcdbremove = keyFun kcdbremove' "kcdbremove"
foreign import ccall "kclangc.h kcdbremove"
  kcdbremove' :: Ptr KCDB -> CString -> CSize -> IO Int32

-------------------------------------------------------------------------------

foreign import ccall "kclangc.h kcdbecode"
  kcdbecode :: Ptr KCDB -> IO Int32

kcdbemsg :: Ptr KCDB -> IO String
kcdbemsg db = kcdbemsg' db >>= peekCString
foreign import ccall "kclangc.h kcdbemsg"
  kcdbemsg' :: Ptr KCDB -> IO CString

-------------------------------------------------------------------------------

data KCException = KCException { excFunction :: String
                               , excError    :: KCError
                               , excMsg      :: String
                               }
                   deriving (Typeable)

instance Show KCException where
  show (KCException fun err msg) = "KyotoCabinet exception, when calling function \"" ++ fun ++ "\". " ++
                                   "Error: " ++ show err ++ ", " ++ msg ++ "."
instance Exception KCException

data KCError = Success | NotImplemented | InvalidOperation | NoRepository
             | NoPermission | BrokenFile | RecordDuplication | NoRecord
             | LogicalInconsistency | SystemError | MiscError
             deriving (Show, Typeable)

getError :: Int32 -> KCError
getError err | err == #{const KCESUCCESS} = Success
             | err == #{const KCENOIMPL}  = NotImplemented
             | err == #{const KCEINVALID} = InvalidOperation
             | err == #{const KCENOREPOS} = NoRepository
             | err == #{const KCENOPERM}  = NoPermission
             | err == #{const KCEBROKEN}  = BrokenFile
             | err == #{const KCEDUPREC}  = RecordDuplication
             | err == #{const KCENOREC}   = NoRecord
             | err == #{const KCELOGIC}   = LogicalInconsistency
             | err == #{const KCESYSTEM}  = SystemError
             | err == #{const KCEMISC}    = MiscError
             | otherwise = error $ "Database.KyotoCabinet.Foreign: received unrecognised error n " ++ show err

throwKCException :: Ptr KCDB -> String -> IO ()
throwKCException db fun =
  throwIO =<< ((KCException fun) <$> fmap getError (kcdbecode db) <*> kcdbemsg db)

handleBoolResult :: Ptr KCDB -> String -> Int32 -> IO ()
handleBoolResult db fun status | status == 0 = throwKCException db fun
                               | otherwise  = return ()

---------------------------------------------------------------------

data KCSTR = KCSTR CString CSize

instance Storable KCSTR where
  sizeOf _ = #{size KCSTR}
  alignment _ = alignment (undefined :: CInt)
  peek ptr =
    do arr <- #{peek KCSTR, buf} ptr
       size <- #{peek KCSTR, size} ptr
       return $ KCSTR arr size
  poke _ _ = error "Database.KyotoCabinet.Foreign.KCSTR.poke not implemented"

withKCSTR :: ByteString -> (KCSTR -> IO a) -> IO a
withKCSTR bs f = BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> f (KCSTR ptr (fi len))
  

---------------------------------------------------------------------

data KCREC = KCREC KCSTR KCSTR

instance Storable KCREC where
  sizeOf _ = #{size KCREC}
  alignment _ = alignment (undefined :: CInt)
  peek ptr =
    do k <- #{peek KCREC, key} ptr
       v <- #{peek KCREC, value} ptr
       return $ KCREC k v
  poke _ _ = error "Database.KyotoCabinet.Foreign.KCREC.poke not implemented"

withKCREC :: ByteString -> ByteString -> (KCREC -> IO a) -> IO a
withKCREC k v f = withKCSTR k $ \kstr ->
                  withKCSTR v $ \vstr ->
                  f $ KCREC kstr vstr
  
