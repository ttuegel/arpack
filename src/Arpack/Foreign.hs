{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Arpack.Foreign
       ( dnaupd, dneupd
       , znaupd, zneupd
       ) where

import Data.Complex
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign
import Foreign.C.String
import Foreign.Storable.Complex ()

import Arpack.State

--
-- * Types
--

type XXaupd t real
  = Ptr Int32     -- ido
    -> CString    -- bmat
    -> Ptr Int32  -- n
    -> CString    -- which
    -> Ptr Int32  -- nev
    -> Ptr real   -- tol
    -> Ptr t      -- resid
    -> Ptr Int32  -- ncv
    -> Ptr t      -- v
    -> Ptr Int32  -- ldv
    -> Ptr Int32  -- iparam
    -> Ptr Int32  -- ipntr
    -> Ptr t      -- workd
    -> Ptr t      -- workl
    -> Ptr Int32  -- lworkl
    -> Ptr real   -- rwork
    -> Ptr Int32  -- info
    -> IO ()

type XXeupd t real
  = Ptr Int32     -- rvec
    -> CString    -- all
    -> Ptr Int32  -- select
    -> Ptr t      -- d
    -> Ptr t      -- z
    -> Ptr Int32  -- ldz
    -> Ptr real   -- sigma
    -> Ptr t      -- workev
    -> CString    -- bmat
    -> Ptr Int32  -- n
    -> CString    -- which
    -> Ptr Int32  -- nev
    -> Ptr real   -- tol
    -> Ptr t      -- resid
    -> Ptr Int32  -- ncv
    -> Ptr t      -- v
    -> Ptr Int32  -- ldv
    -> Ptr Int32  -- iparam
    -> Ptr Int32  -- ipntr
    -> Ptr t      -- workd
    -> Ptr t      -- workl
    -> Ptr Int32  -- lworkl
    -> Ptr real   -- rwork
    -> Ptr Int32  -- ierr
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
dnaupd (AUPD {..}) =
  VSM.unsafeWith resid $ \_resid ->
  VSM.unsafeWith v $ \_v ->
  VSM.unsafeWith iparam $ \_iparam ->
  VSM.unsafeWith ipntr $ \_ipntr ->
  VSM.unsafeWith workd $ \_workd ->
  VSM.unsafeWith workl $ \_workl ->
  VSM.unsafeWith rwork $ \_rwork ->
    dnaupd_ ido bmat n which nev tol _resid ncv _v ldv _iparam _ipntr
            _workd _workl lworkl _rwork info

dneupd :: EUPD Double -> AUPD Double -> IO ()
dneupd (EUPD {..}) (AUPD {..}) =
  VSM.unsafeWith select $ \_select ->
  VSM.unsafeWith d $ \_d ->
  VSM.unsafeWith z $ \_z ->
  VSM.unsafeWith workev $ \_workev ->
  VSM.unsafeWith resid $ \_resid ->
  VSM.unsafeWith v $ \_v ->
  VSM.unsafeWith iparam $ \_iparam ->
  VSM.unsafeWith ipntr $ \_ipntr ->
  VSM.unsafeWith workd $ \_workd ->
  VSM.unsafeWith workl $ \_workl ->
  VSM.unsafeWith rwork $ \_rwork ->
    dneupd_ rvec howmny _select _d _z ldz sigma _workev
            bmat n which nev tol _resid ncv _v ldv _iparam _ipntr
            _workd _workl lworkl _rwork info

znaupd :: AUPD (Complex Double) -> IO ()
znaupd (AUPD {..}) =
  VSM.unsafeWith resid $ \_resid ->
  VSM.unsafeWith v $ \_v ->
  VSM.unsafeWith iparam $ \_iparam ->
  VSM.unsafeWith ipntr $ \_ipntr ->
  VSM.unsafeWith workd $ \_workd ->
  VSM.unsafeWith workl $ \_workl ->
  VSM.unsafeWith rwork $ \_rwork ->
    znaupd_ ido bmat n which nev tol _resid ncv _v ldv _iparam _ipntr
            _workd _workl lworkl _rwork info

zneupd :: EUPD (Complex Double) -> AUPD (Complex Double) -> IO ()
zneupd (EUPD {..}) (AUPD {..}) =
  VSM.unsafeWith select $ \_select ->
  VSM.unsafeWith d $ \_d ->
  VSM.unsafeWith z $ \_z ->
  VSM.unsafeWith workev $ \_workev ->
  VSM.unsafeWith resid $ \_resid ->
  VSM.unsafeWith v $ \_v ->
  VSM.unsafeWith iparam $ \_iparam ->
  VSM.unsafeWith ipntr $ \_ipntr ->
  VSM.unsafeWith workd $ \_workd ->
  VSM.unsafeWith workl $ \_workl ->
  VSM.unsafeWith rwork $ \_rwork ->
    zneupd_ rvec howmny _select _d _z ldz sigma _workev
            bmat n which nev tol _resid ncv _v ldv _iparam _ipntr
            _workd _workl lworkl _rwork info
