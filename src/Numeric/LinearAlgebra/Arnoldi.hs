{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Numeric.LinearAlgebra.Arnoldi
       ( Arpack(..)
       , Options(..), Which(..)
       ) where

import qualified Control.Concurrent.Lock as Lock
import Control.Exception (throwIO)
import Data.Complex
import Data.Maybe (fromMaybe)
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

    -- shift strategy
    VSM.write iparam 0 1

    -- maximum number of iterations
    VSM.write iparam 2 (fromIntegral (fromMaybe (3 * dim) (maxIterations opts)))

    -- block size
    VSM.write iparam 3 1

    -- eigenproblem type
    VSM.write iparam 6 1

    let
      loop = do
        aupd stateA
        i <- peek ido
        case i of
          99 -> pure ()

          _ | abs i == 1 -> do
                xi <- fromIntegral <$> VSM.read ipntr 0
                let
                  x = VSM.slice (xi - 1) dim workd
                yi <- fromIntegral <$> VSM.read ipntr 1
                let
                  y = VSM.slice (yi - 1) dim workd
                multiply y x
                loop

            | otherwise -> throwIO Impossible

      messages = do
        i <- peek info
        case i of
          0 -> pure ()
          1 -> throwIO MaxIterations
          3 -> throwIO NoShifts
          _ -> printf "znaupd: info = %d" (fromIntegral i :: Int)

      extract = withEUPD stateA $ \stateE@(EUPD {..}) -> do
        eupd stateE stateA

        i <- peek info
        case i of
          0 -> pure ()
          _ -> throwIO Impossible

        evals <- VS.unsafeFreeze (VSM.slice 0 (number opts) d)
        evecs <- VS.unsafeFreeze (VSM.slice 0 (number opts * dim) z)
        pure (evals, Dense.matrixFromVector Dense.ColumnMajor dim (number opts) evecs)

    Lock.with Arpack.lock (loop >> messages >> extract)
