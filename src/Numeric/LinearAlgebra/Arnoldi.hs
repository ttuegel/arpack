{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Numeric.LinearAlgebra.Arnoldi
       ( Arpack, eig
         -- * Options
       , Options(..), Which(..)
         -- * Exceptions
       , MaxIterations(..)
       , NoShifts(..)
       , Reallocate(..)
       , XYAUPD(..)
       , XYEUPD(..)
       , Unimplemented(..)
       ) where

import Data.Vector.Storable (Vector)
import Data.Vector.Storable.Mutable (IOVector)
import Numeric.LinearAlgebra (Matrix)
import System.IO.Unsafe (unsafePerformIO)

import Arpack.Exceptions
import Arpack.Foreign
import Arpack.Options

eig :: Arpack t => Options t -> Int -> (IOVector t -> IOVector t -> IO ())
    -> (Vector t, Matrix t)
eig !options !dim !multiply = unsafePerformIO (arpack options dim multiply)
