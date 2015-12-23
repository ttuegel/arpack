{-# LANGUAGE RecordWildCards #-}

module Numeric.LinearAlgebra.Arnoldi.State where

import Control.Exception (bracket)
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Options

data AUPD t = AUPD
              { ido :: {-# UNPACK #-} !(Ptr CInt)
              , bmat :: {-# UNPACK #-} !CString
              , n :: {-# UNPACK #-} !(Ptr CInt)
              , which :: {-# UNPACK #-} !CString
              , nev :: {-# UNPACK #-} !(Ptr CInt)
              , tol :: {-# UNPACK #-} !(Ptr CDouble)
              , resid :: {-# UNPACK #-} !(IOVector t)
              , ncv :: {-# UNPACK #-} !(Ptr CInt)
              , v :: {-# UNPACK #-} !(IOVector t)
              , ldv :: {-# UNPACK #-} !(Ptr CInt)
              , iparam :: {-# UNPACK #-} !(IOVector CInt)
              , ipntr :: {-# UNPACK #-} !(IOVector CInt)
              , workd :: {-# UNPACK #-} !(IOVector t)
              , workl :: {-# UNPACK #-} !(IOVector t)
              , lworkl :: {-# UNPACK #-} !(Ptr CInt)
              , rwork :: {-# UNPACK #-} !(IOVector Double)
              , info :: {-# UNPACK #-} !(Ptr CInt)
              }

withAUPD :: Storable t => Options t -> Int -> (AUPD t -> IO a) -> IO a
withAUPD options dim = bracket (initAUPD options dim) freeAUPD

initAUPD :: Storable t => Options t -> Int -> IO (AUPD t)
initAUPD options dim = do
  let
    _nev = number options

    -- Largest number of basis vectors to use.
    -- Work per iteration is O(dim * ncv ^ 2).

    _ncv = min dim (4 * _nev)

    _lworkl = 3 * _ncv * _ncv + 5 * _ncv

  ido <- new 0
  bmat <- newCString "I"
  n <- new (fromIntegral dim)
  which <- newCString "SR"
  nev <- new (fromIntegral _nev)
  tol <- new 0
  resid <- VSM.new dim
  ncv <- new (fromIntegral _ncv)
  v <- VSM.new (dim * _ncv)
  ldv <- new (fromIntegral dim)
  iparam <- VSM.new 11
  ipntr <- VSM.new 14
  workd <- VSM.new (3 * dim)
  workl <- VSM.new _lworkl
  lworkl <- new (fromIntegral _lworkl)
  rwork <- VSM.new _ncv
  info <- new 0

  pure AUPD {..}

freeAUPD :: AUPD t -> IO ()
freeAUPD AUPD {..} = do
  free ido
  free bmat
  free n
  free which
  free nev
  free tol
  free ncv
  free ldv
  free lworkl
  free info

data EUPD t = EUPD
              { rvec :: {-# UNPACK #-} !(Ptr CInt)
              , howmny :: {-# UNPACK #-} !CString
              , select :: {-# UNPACK #-} !(IOVector CInt)
              , d :: {-# UNPACK #-} !(IOVector t)
              , z :: {-# UNPACK #-} !(IOVector t)
              , ldz :: {-# UNPACK #-} !(Ptr CInt)
              , sigma :: {-# UNPACK #-} !(Ptr t)
              , workev :: {-# UNPACK #-} !(IOVector t)
              }

withEUPD :: Storable t => Options t -> AUPD t -> (EUPD t -> IO a) -> IO a
withEUPD options aupd = bracket (initEUPD options aupd) freeEUPD

initEUPD :: Storable t => Options t -> AUPD t -> IO (EUPD t)
initEUPD options (AUPD {..}) = do
  let nev = number options
  _ncv <- fromIntegral <$> peek ncv
  dim <- fromIntegral <$> peek n

  rvec <- new (if findVectors options then 1 else 0)
  howmny <- newCString "A"
  select <- VSM.new _ncv
  d <- VSM.new (nev + 1)
  z <- VSM.new (dim * nev)
  ldz <- new (fromIntegral dim)
  sigma <- malloc
  workev <- VSM.new (2 * _ncv)
  pure EUPD {..}

freeEUPD :: EUPD t -> IO ()
freeEUPD (EUPD {..}) = do
  free rvec
  free howmny
  free ldz
  free sigma
