{-# LANGUAGE DeriveDataTypeable #-}

module Arpack.Exceptions where

import Control.Exception
import Data.Typeable

data MaxIterations = MaxIterations deriving (Show, Typeable)

instance Exception MaxIterations

data NoShifts = NoShifts deriving (Show, Typeable)

instance Exception NoShifts

data Impossible = Impossible deriving (Show, Typeable)

instance Exception Impossible

data Reallocate = Reallocate deriving (Show, Typeable)

instance Exception Reallocate
