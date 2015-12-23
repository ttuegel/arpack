{-# LANGUAGE RecordWildCards #-}

module Arpack.State where

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign
import Foreign.C.String

import Arpack.Options

data AUPD t = AUPD
              { ido :: {-# UNPACK #-} !(Ptr Int32)
              , bmat :: {-# UNPACK #-} !CString
              , n :: {-# UNPACK #-} !(Ptr Int32)
              , which :: {-# UNPACK #-} !CString
              , nev :: {-# UNPACK #-} !(Ptr Int32)
              , tol :: {-# UNPACK #-} !(Ptr Double)
              , resid :: {-# UNPACK #-} !(IOVector t)
              , ncv :: {-# UNPACK #-} !(Ptr Int32)
              , v :: {-# UNPACK #-} !(IOVector t)
              , ldv :: {-# UNPACK #-} !(Ptr Int32)
              , iparam :: {-# UNPACK #-} !(IOVector Int32)
              , ipntr :: {-# UNPACK #-} !(IOVector Int32)
              , workd :: {-# UNPACK #-} !(IOVector t)
              , workl :: {-# UNPACK #-} !(IOVector t)
              , lworkl :: {-# UNPACK #-} !(Ptr Int32)
              , rwork :: {-# UNPACK #-} !(IOVector Double)
              , info :: {-# UNPACK #-} !(Ptr Int32)
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
  -- shift strategy
  VSM.write iparam (1 - 1) 1
  -- maximum number of iterations
  VSM.write iparam (3 - 1)
    (fromIntegral (fromMaybe (3 * dim) (maxIterations options)))
  -- block size
  VSM.write iparam (4 - 1) 1
  -- eigenproblem type
  VSM.write iparam (7 - 1) 1

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
              { rvec :: {-# UNPACK #-} !(Ptr Int32)
              , howmny :: {-# UNPACK #-} !CString
              , select :: {-# UNPACK #-} !(IOVector Int32)
              , d :: {-# UNPACK #-} !(IOVector t)
              , z :: {-# UNPACK #-} !(IOVector t)
              , ldz :: {-# UNPACK #-} !(Ptr Int32)
              , sigma :: {-# UNPACK #-} !(Ptr Double)
              , workev :: {-# UNPACK #-} !(IOVector t)
              }

withEUPD :: Storable t => AUPD t -> (EUPD t -> IO a) -> IO a
withEUPD aupd = bracket (initEUPD aupd) freeEUPD

initEUPD :: Storable t => AUPD t -> IO (EUPD t)
initEUPD (AUPD {..}) = do
  _nev <- fromIntegral <$> peek nev
  _ncv <- fromIntegral <$> peek ncv
  dim <- fromIntegral <$> peek n

  rvec <- new 1
  howmny <- newCString "A"
  select <- VSM.new _ncv
  d <- VSM.new (_nev + 1)
  z <- VSM.new (dim * _nev)
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
