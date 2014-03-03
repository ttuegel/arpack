{-# LANGUAGE ForeignFunctionInterface #-}

module Internal where

import Data.Complex
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable.Complex ()

-- Types ----------

type XXaupd t real
        = Ptr CInt      -- ido
          -> CString    -- bmat
          -> Ptr CInt   -- n
          -> CString    -- which
          -> Ptr CInt   -- nev
          -> Ptr real   -- tol
          -> Ptr t      -- resid
          -> Ptr CInt   -- ncv
          -> Ptr t      -- v
          -> Ptr CInt   -- ldv
          -> Ptr CInt   -- iparam
          -> Ptr CInt   -- ipntr
          -> Ptr t      -- workd
          -> Ptr t      -- workl
          -> Ptr CInt   -- lworkl
          -> Ptr real   -- rwork
          -> Ptr CInt   -- info
          -> IO ()

type XXeupd t real
        = Ptr CInt      -- rvec
          -> CString    -- all
          -> Ptr CInt   -- select
          -> Ptr t      -- d
          -> Ptr t      -- z
          -> Ptr CInt   -- ldz
          -> Ptr real   -- sigma
          -> Ptr t      -- workev
          -> CString    -- bmat
          -> Ptr CInt   -- n
          -> CString    -- which
          -> Ptr CInt   -- nev
          -> Ptr real   -- tol
          -> Ptr t      -- resid
          -> Ptr CInt   -- ncv
          -> Ptr t      -- v
          -> Ptr CInt   -- ldv
          -> Ptr CInt   -- iparam
          -> Ptr CInt   -- ipntr
          -> Ptr t      -- workd
          -> Ptr t      -- workl
          -> Ptr CInt   -- lworkl
          -> Ptr real   -- rwork
          -> Ptr CInt   -- ierr
          -> IO ()

-- Foreign imports ----------

foreign import ccall unsafe "snaupd_" snaupd_ :: XXaupd Float Float
foreign import ccall unsafe "sneupd_" sneupd_ :: XXeupd Float Float
foreign import ccall unsafe "dnaupd_" dnaupd_ :: XXaupd Double Double
foreign import ccall unsafe "dneupd_" dneupd_ :: XXeupd Double Double

foreign import ccall unsafe "cnaupd_" cnaupd_ :: XXaupd (Complex Float) Float
foreign import ccall unsafe "cneupd_" cneupd_ :: XXeupd (Complex Float) Float

foreign import ccall unsafe "znaupd_" znaupd_ :: XXaupd (Complex Double) Double
foreign import ccall unsafe "zneupd_" zneupd_ :: XXeupd (Complex Double) Double
