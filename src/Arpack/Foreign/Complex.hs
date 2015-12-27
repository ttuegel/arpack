{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arpack.Foreign.Complex () where

import qualified Control.Concurrent.Lock as Lock
import Control.Exception (bracket, throwIO)
import Data.Complex
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign
import Foreign.C.String
import Foreign.C.Types (CChar)
import Foreign.Storable.Complex ()
import qualified Numeric.LinearAlgebra.Devel as Dense

import Arpack.Exceptions
import Arpack.Foreign.Class
import qualified Arpack.Lock as Arpack
import Arpack.Options

--
-- * Foreign functions
--

foreign import ccall unsafe "znaupd_"
  znaupd_ :: Ptr Int32            -- ido
          -> Ptr CChar            -- bmat
          -> Ptr Int32            -- n
          -> Ptr CChar            -- which
          -> Ptr Int32            -- nev
          -> Ptr Double           -- tol
          -> Ptr (Complex Double) -- resid
          -> Ptr Int32            -- ncv
          -> Ptr (Complex Double) -- v
          -> Ptr Int32            -- ldv
          -> Ptr Int32            -- iparam
          -> Ptr Int32            -- ipntr
          -> Ptr (Complex Double) -- workd
          -> Ptr (Complex Double) -- workl
          -> Ptr Int32            -- lworkl
          -> Ptr Double           -- rwork
          -> Ptr Int32            -- info
          -> IO ()

foreign import ccall unsafe "zneupd_"
  zneupd_ :: Ptr Int32            -- rvec
          -> Ptr CChar            -- howmny
          -> Ptr Int32            -- select
          -> Ptr (Complex Double) -- d
          -> Ptr (Complex Double) -- z
          -> Ptr Int32            -- ldz
          -> Ptr (Complex Double) -- sigma
          -> Ptr (Complex Double) -- workev
          -> Ptr CChar            -- bmat
          -> Ptr Int32            -- n
          -> Ptr CChar            -- which
          -> Ptr Int32            -- nev
          -> Ptr Double           -- tol
          -> Ptr (Complex Double) -- resid
          -> Ptr Int32            -- ncv
          -> Ptr (Complex Double) -- v
          -> Ptr Int32            -- ldv
          -> Ptr Int32            -- iparam
          -> Ptr Int32            -- ipntr
          -> Ptr (Complex Double) -- workd
          -> Ptr (Complex Double) -- workl
          -> Ptr Int32            -- lworkl
          -> Ptr Double           -- rwork
          -> Ptr Int32            -- info
          -> IO ()

--
-- * Wrappers
--


data AUPD
  = AUPD
    { ido :: {-# UNPACK #-} !(Ptr Int32)
    , bmat :: {-# UNPACK #-} !CString
    , n :: {-# UNPACK #-} !(Ptr Int32)
    , which :: {-# UNPACK #-} !CString
    , nev :: {-# UNPACK #-} !(Ptr Int32)
    , tol :: {-# UNPACK #-} !(Ptr Double)
    , resid :: {-# UNPACK #-} !(IOVector (Complex Double))
    , ncv :: {-# UNPACK #-} !(Ptr Int32)
    , v :: {-# UNPACK #-} !(IOVector (Complex Double))
    , ldv :: {-# UNPACK #-} !(Ptr Int32)
    , iparam :: {-# UNPACK #-} !(IOVector Int32)
    , ipntr :: {-# UNPACK #-} !(IOVector Int32)
    , workd :: {-# UNPACK #-} !(IOVector (Complex Double))
    , workl :: {-# UNPACK #-} !(IOVector (Complex Double))
    , lworkl :: {-# UNPACK #-} !(Ptr Int32)
    , rwork :: {-# UNPACK #-} !(IOVector Double)
    , info :: {-# UNPACK #-} !(Ptr Int32)
    }

withAUPD :: Options (Complex Double) -> Int -> (AUPD -> IO a) -> IO a
withAUPD options dim = bracket initAUPD freeAUPD where

  initAUPD = do
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

aupd :: AUPD -> IO ()
aupd (AUPD {..}) =
  VSM.unsafeWith resid $ \_resid ->
  VSM.unsafeWith v $ \_v ->
  VSM.unsafeWith iparam $ \_iparam ->
  VSM.unsafeWith ipntr $ \_ipntr ->
  VSM.unsafeWith workd $ \_workd ->
  VSM.unsafeWith workl $ \_workl ->
  VSM.unsafeWith rwork $ \_rwork ->

  znaupd_ ido bmat n which nev tol _resid ncv _v ldv _iparam _ipntr
          _workd _workl lworkl _rwork info

data EUPD
  = EUPD
    { rvec :: {-# UNPACK #-} !(Ptr Int32)
    , howmny :: {-# UNPACK #-} !CString
    , select :: {-# UNPACK #-} !(IOVector Int32)
    , d :: {-# UNPACK #-} !(IOVector (Complex Double))
    , z :: {-# UNPACK #-} !(IOVector (Complex Double))
    , ldz :: {-# UNPACK #-} !(Ptr Int32)
    , sigma :: {-# UNPACK #-} !(Ptr (Complex Double))
    , workev :: {-# UNPACK #-} !(IOVector (Complex Double))
    }

withEUPD :: AUPD -> (EUPD -> IO a) -> IO a
withEUPD (AUPD {..}) = bracket initEUPD freeEUPD where

  initEUPD = do
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

  freeEUPD (EUPD {..}) = do
    free rvec
    free howmny
    free ldz
    free sigma

eupd :: EUPD -> AUPD -> IO ()
eupd (EUPD {..}) (AUPD {..}) =
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

instance Arpack (Complex Double) where

  arpack !opts !dim !multiply
    -- These variables are all banged because we need to be strict
    -- in them _before_ we enter the locked segment of code! If we
    -- wait until we're inside the lock, and evaluating one of these
    -- variables invokes 'arpack' again, the program will deadlock!
    = withAUPD opts dim $ \stateA@(AUPD {..}) -> do

      let
        loop = do
          aupd stateA
          peek ido >>= \case
            99 -> do
              peek info >>= \case
                0 -> pure ()
                1 -> throwIO MaxIterations
                3 -> throwIO NoShifts
                i -> throwIO (XYAUPD i)

            i | abs i == 1 -> do
                  xi <- fromIntegral <$> VSM.read ipntr 0
                  let
                    x = VSM.slice (xi - 1) dim workd
                  yi <- fromIntegral <$> VSM.read ipntr 1
                  let
                    y = VSM.slice (yi - 1) dim workd
                  multiply y x
                  loop

              | otherwise -> throwIO (Unimplemented i)

        extract = withEUPD stateA $ \stateE@(EUPD {..}) -> do
          eupd stateE stateA

          peek info >>= \case
            0 -> pure ()
            1 -> throwIO Reallocate
            i -> throwIO (XYEUPD i)

          evals <- VS.unsafeFreeze (VSM.slice 0 (number opts) d)
          evecs <- VS.unsafeFreeze (VSM.slice 0 (number opts * dim) z)
          let matrixFromVector = Dense.matrixFromVector Dense.ColumnMajor
          pure (evals, matrixFromVector dim (number opts) evecs)

      Lock.with Arpack.lock (loop >> extract)
