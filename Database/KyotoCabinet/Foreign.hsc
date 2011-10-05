{-# Language ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}
module Database.KyotoCabinet.Foreign where

import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception, throwIO)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Data (Typeable)
import Data.Int (Int32, Int64)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CSize, CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen, peekArray, allocaArray)
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


newtype KCSTR = KCSTR {unKCSTR :: (CString, CSize)}

instance Storable KCSTR where
  sizeOf _ = #{size KCSTR}
  alignment _ = alignment (undefined :: CInt)
  peek ptr =
    do arr <- #{peek KCSTR, buf} ptr
       size <- #{peek KCSTR, size} ptr
       return $ KCSTR (arr, size)
  poke _ _ = error "Database.KyotoCabinet.Foreign.KCSTR.poke not implemented"

withKCSTR :: ByteString -> (KCSTR -> IO a) -> IO a
withKCSTR bs f = BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> f (KCSTR (ptr, (fi len)))

withKCSTRArray :: [ByteString] -> (CSize -> Ptr KCSTR -> IO a) -> IO a
withKCSTRArray ks f = go ks [] $ \kcstrs -> withArrayLen kcstrs (f . fi)
  where
    go []        kcstrs f' = f' $ reverse kcstrs
    go (k : ks') kcstrs f' = withKCSTR k $ \kcstr -> go ks' (kcstr : kcstrs) f'

---------------------------------------------------------------------

newtype KCREC = KCREC {unKCREC :: (ByteString, ByteString)}

instance Storable KCREC where
  sizeOf _ = #{size KCREC}
  alignment _ = alignment (undefined :: CInt)
  peek ptr =
    do k <- #{peek KCREC, key} ptr >>= BS.unsafePackCStringLen . second fi . unKCSTR
       v <- #{peek KCREC, value} ptr >>= BS.unsafePackCStringLen . second fi . unKCSTR
       return $ KCREC (k, v)
  poke _ _ = error "Database.KyotoCabinet.Foreign.KCREC.poke not implemented"

withKCRECArray :: [(ByteString, ByteString)] -> (CSize -> Ptr KCREC -> IO a) -> IO a
withKCRECArray kvs f = withArrayLen (map KCREC kvs) (f . fi)

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
  mkKCVISITEMPTY :: (CString -> CSize -> Ptr CSize -> Ptr () -> IO CString) -> IO KCVISITEMPTY

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

getFun :: (Ptr KCDB -> CString -> CSize -> Ptr CSize -> IO CString)
          -> (Ptr KCDB -> ByteString -> IO (Maybe ByteString))
getFun f db k =
  BS.unsafeUseAsCStringLen k $ \(kptr, klen) ->
  alloca $ \vlenptr ->
  do vptr <- f db kptr (fi klen) vlenptr
     if vptr == nullPtr then return Nothing
       else peek vlenptr >>= \vlen -> fmap Just $ BS.unsafePackCStringLen (vptr, fi vlen)

-------------------------------------------------------------------------------

data KCDB

-------------------------------------------------------------------------------

kcdbnew :: IO (Ptr KCDB)
kcdbnew =
  do kcdb <- kcdbnew'
     if kcdb == nullPtr then
       error "Database.KyotoCabinet.Foreign.kcdbnew: kcdbnew returned NULL."
       else return kcdb
foreign import ccall "kclangc.h kcdbnew"
  kcdbnew' :: IO (Ptr KCDB)

foreign import ccall "kclangc.h kcdbdel"
  kcdbdel :: Ptr KCDB -> IO ()

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

foreign import ccall "kclangc.h kcdbecode"
  kcdbecode :: Ptr KCDB -> IO Int32

kcdbemsg :: Ptr KCDB -> IO String
kcdbemsg db = kcdbemsg' db >>= peekCString
foreign import ccall "kclangc.h kcdbemsg"
  kcdbemsg' :: Ptr KCDB -> IO CString

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
  withKCSTRArray ks $ \len kcstrptr ->
  do vfptr <- mkVisitorFull vf
     veptr <- mkVisitorEmpty ve
     kcdbacceptbulk' db kcstrptr (fi len) vfptr veptr nullPtr (boolToInt w) >>= handleBoolResult db "kcdbacceptbulk"
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

kcdbappend :: Ptr KCDB -> ByteString -> ByteString -> IO ()
kcdbappend = keyValFun kcdbappend' "kcdbappend"
foreign import ccall "kclangc.h kcdbappend"
  kcdbappend' :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO Int32

kcdbremove :: Ptr KCDB -> ByteString -> IO ()
kcdbremove = keyFun kcdbremove' "kcdbremove"
foreign import ccall "kclangc.h kcdbremove"
  kcdbremove' :: Ptr KCDB -> CString -> CSize -> IO Int32

kcdbget :: Ptr KCDB -> ByteString -> IO (Maybe ByteString)
kcdbget = getFun kcdbget'
foreign import ccall "kclangc.h kcdbget"
  kcdbget' :: Ptr KCDB -> CString -> CSize -> Ptr CSize -> IO CString

-- int32_t kcdbgetbuf (KCDB *db, const char *kbuf, size_t ksiz, char *vbuf, size_t max)

-- char *kcdbseize (KCDB *db, const char *kbuf, size_t ksiz, size_t *sp)

kcdbseize :: Ptr KCDB -> ByteString -> IO (Maybe ByteString)
kcdbseize = getFun kcdbseize'
foreign import ccall "kclangc.h kcdbseize"
  kcdbseize' :: Ptr KCDB -> CString -> CSize -> Ptr CSize -> IO CString

kcdbsetbulk :: Ptr KCDB -> [(ByteString, ByteString)] -> Bool -> IO Int64
kcdbsetbulk db kvs atomic =
  withKCRECArray kvs $ \len kcrecptr ->
  kcdbsetbulk' db kcrecptr len (boolToInt atomic) >>= handleCountResult db "kcdbsetbulk"
foreign import ccall "kclangc.h kcdbsetbulk"
  kcdbsetbulk' :: Ptr KCDB -> Ptr KCREC -> CSize -> Int32 -> IO Int64

kcdbremovebulk :: Ptr KCDB -> [ByteString] -> Bool -> IO Int64
kcdbremovebulk db ks atomic =
  withKCSTRArray ks $ \len kcstrptr ->
  kcdbremovebulk' db kcstrptr len (boolToInt atomic) >>= handleCountResult db "kcdbremovebulk"
foreign import ccall "kclangc.h kcdbremovebulk"
  kcdbremovebulk' :: Ptr KCDB -> Ptr KCSTR -> CSize -> Int32 -> IO Int64

-- int64_t kcdbgetbulk (KCDB *db, const KCSTR *keys, size_t knum, KCREC *recs, int32_t atomic)

kcdbgetbulk :: Ptr KCDB -> [ByteString] -> Bool -> IO [(ByteString, ByteString)]
kcdbgetbulk db ks atomic =
  withKCSTRArray ks $ \len kcstrptr ->
  allocaArray (fi len) $ \kcrecptr ->
  do count <- kcdbgetbulk' db kcstrptr len kcrecptr (boolToInt atomic) >>=
              handleCountResult db "kcdbgetbulk"
     kcrecs <- peekArray (fi count) kcrecptr
     return $ map unKCREC kcrecs
foreign import ccall "kclangc.h kcdbgetbulk"
  kcdbgetbulk' :: Ptr KCDB -> Ptr KCSTR -> CSize -> Ptr KCREC -> Int32 -> IO Int64

-- int32_t kcdbsync (KCDB *db, int32_t hard, KCFILEPROC proc, void *opq)

-- int32_t kcdboccupy (KCDB *db, int32_t writable, KCFILEPROC proc, void *opq)

-- int32_t kcdbcopy (KCDB *db, const char *dest)

-- int32_t kcdbbegintran (KCDB *db, int32_t hard)

-- int32_t kcdbbegintrantry (KCDB *db, int32_t hard)

-- int32_t kcdbendtran (KCDB *db, int32_t commit)

-- int32_t kcdbclear (KCDB *db)

-- int32_t kcdbdumpsnap (KCDB *db, const char *dest)

-- int32_t kcdbloadsnap (KCDB *db, const char *src)

-- int64_t kcdbcount (KCDB *db)

-- int64_t kcdbsize (KCDB *db)

-- char *kcdbpath (KCDB *db)

-- char *kcdbstatus (KCDB *db)

-- int64_t kcdbmatchprefix (KCDB *db, const char *prefix, char **strary, size_t max)

-- int64_t kcdbmatchregex (KCDB *db, const char *regex, char **strary, size_t max)

-- int32_t kcdbmerge (KCDB *db, KCDB **srcary, size_t srcnum, uint32_t mode)

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

throwKCException :: Ptr KCDB -> String -> IO a
throwKCException db fun =
  throwIO =<< ((KCException fun) <$> fmap getError (kcdbecode db) <*> kcdbemsg db)

handleBoolResult :: Ptr KCDB -> String -> Int32 -> IO ()
handleBoolResult db fun status | status == 0 = throwKCException db fun
                               | otherwise  = return ()

handleCountResult :: Ptr KCDB -> String -> Int64 -> IO Int64
handleCountResult db fun count | count == -1 = throwKCException db fun
                               | otherwise  = return count


---------------------------------------------------------------------
