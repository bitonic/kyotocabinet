{-# Language ForeignFunctionInterface, EmptyDataDecls #-}
module Database.KyotoCabinet.Foreign
       ( KCDB
         -- * Opening/creating/closing
       , kcdbnew
       , kcdbopen
       , kcdbclose
         -- ** Open modes
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
         -- * Error handling
       , kcdbecode
       , kcdbemsg
       ) where

import Data.Int (Int32)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr)

#include <kclangc.h>

data KCDB

-------------------------------------------------------------------------------

type OpenMode = Int32

#{enum OpenMode,,
 _KCOREADER = KCOREADER,
 _KCOWRITER = KCOWRITER,
 _KCOCREATE = KCOCREATE,
 _KCOTRUNCATE = KCOTRUNCATE,
 _KCOAUTOTRAN = KCOAUTOTRAN,
 _KCOAUTOSYNC = KCOAUTOSYNC,
 _KCONOLOCK = KCONOLOCK,
 _KCOTRYLOCK = KCOTRYLOCK,
 _KCONOREPAIR = KCONOREPAIR
 }

foreign import ccall "kclangc.h kcdbnew"
  kcdbnew :: IO (Ptr KCDB)

foreign import ccall "kclangc.h kcdbopen"
  kcdbopen :: Ptr KCDB
              -> CString  -- ^ File name
              -> Int32    -- ^ Open mode
              -> IO Int32

foreign import ccall "kclangc.h kcdbclose"
  kcdbclose :: Ptr KCDB -> IO Int32

-------------------------------------------------------------------------------

foreign import ccall "kclangc.h kcdbset"
  kcdbset :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO Int32

foreign import ccall "kclangc.h kcdbget"
  kcdbget :: Ptr KCDB -> CString -> CSize -> Ptr CSize -> IO CString

-------------------------------------------------------------------------------

foreign import ccall "kclangc.h kcdbecode"
  kcdbecode :: Ptr KCDB -> IO Int32

foreign import ccall "kclangc.h kcdbemsg"
  kcdbemsg :: Ptr KCDB -> IO Int32
