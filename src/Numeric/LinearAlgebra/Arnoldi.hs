{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Numeric.LinearAlgebra.Arnoldi
       ( Arpack(..)
       , Options(..), Which(..)
       ) where

import qualified Control.Concurrent.Lock as Lock
import Control.Exception (throwIO)
import Data.Complex
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS hiding (unsafeWith)
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign (Storable, peek)
import Numeric.LinearAlgebra (Matrix)
import qualified Numeric.LinearAlgebra.Devel as Dense
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

import Arpack.Exceptions
import Arpack.Foreign
import qualified Arpack.Lock as Arpack
import Arpack.Options
import Arpack.State

class Arpack t where
  eig :: Options t -> Int -> (IOVector t -> IOVector t -> IO ())
      -> (Vector t, Matrix t)

instance Arpack Double where
  eig = wrapper dnaupd dneupd

instance Arpack (Complex Double) where
  eig = wrapper znaupd zneupd

wrapper :: (Storable t)
        => (AUPD t -> IO ()) -> (EUPD t -> AUPD t -> IO ())
        -> Options t
        -> Int  -- ^ dimension of linear operator
        -> (IOVector t -> IOVector t -> IO ())  -- ^ operator
        -> (Vector t, Matrix t)
wrapper aupd eupd !opts !dim !multiply
  -- These variables are all banged because we need to be strict
  -- in them _before_ we enter the locked segment of code! If we
  -- wait until we're inside the lock, and evaluating one of these
  -- variables invokes 'arpack' again, the program will deadlock!
  = unsafePerformIO $ withAUPD opts dim $ \stateA@(AUPD {..}) -> do

    VS.freeze iparam >>= print
 
    let
      loop = do
        aupd stateA
        peek ido >>= \case
          99 -> do
            peek info >>= \case
              0 -> VS.freeze iparam >>= print >> pure ()
              1 -> throwIO MaxIterations
              3 -> throwIO NoShifts
              i -> do
                printf "xyaupd: info = %d\n" (fromIntegral i :: Int)
                throwIO Impossible

          i | abs i == 1 -> do
                xi <- fromIntegral <$> VSM.read ipntr 0
                let
                  x = VSM.slice (xi - 1) dim workd
                yi <- fromIntegral <$> VSM.read ipntr 1
                let
                  y = VSM.slice (yi - 1) dim workd
                multiply y x
                loop

            | otherwise -> throwIO Impossible

      extract = withEUPD stateA $ \stateE@(EUPD {..}) -> do
        eupd stateE stateA

        peek info >>= \case
          0 -> pure ()
          1 -> throwIO Reallocate
          i -> do
            printf "xyeupd: info = %d\n" (fromIntegral i :: Int)
            throwIO Impossible

        evals <- VS.unsafeFreeze (VSM.slice 0 (number opts) d)
        evecs <- VS.unsafeFreeze (VSM.slice 0 (number opts * dim) z)
        let matrixFromVector = Dense.matrixFromVector Dense.ColumnMajor
        pure (evals, matrixFromVector dim (number opts) evecs)

    Lock.with Arpack.lock (loop >> extract)
