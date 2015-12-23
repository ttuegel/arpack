{-# LANGUAGE ForeignFunctionInterface #-}

module Internal
       ( dnaupd, dneupd
       , znaupd, zneupd
       ) where

import Data.Complex
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable.Complex ()

import Numeric.LinearAlgebra.Arnoldi.State

--
-- * Types
--

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

--
-- * Foreign functions
--

foreign import ccall unsafe "dnaupd_" dnaupd_ :: XXaupd Double Double
foreign import ccall unsafe "dneupd_" dneupd_ :: XXeupd Double Double

foreign import ccall unsafe "znaupd_" znaupd_ :: XXaupd (Complex Double) Double
foreign import ccall unsafe "zneupd_" zneupd_ :: XXeupd (Complex Double) Double

--
-- * Wrappers
--

dnaupd :: AUPD Double -> IO ()
dnaupd = _

dneupd :: EUPD Double -> AUPD Double -> IO ()
dneupd = _

znaupd :: AUPD (Complex Double) -> IO ()
znaupd = _

zneupd :: EUPD (Complex Double) -> AUPD (Complex Double) -> IO ()
zneupd = _
