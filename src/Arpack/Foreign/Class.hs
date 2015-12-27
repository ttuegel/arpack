{-# LANGUAGE RecordWildCards #-}

module Arpack.Foreign.Class where

import Data.Vector.Storable.Mutable (IOVector)
import Numeric.LinearAlgebra (Matrix, Vector)

import Arpack.Options

class Arpack t where
  arpack :: Options t -> Int -> (IOVector t -> IOVector t -> IO ())
         -> IO (Vector t, Matrix t)
