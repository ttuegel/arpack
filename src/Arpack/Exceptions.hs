{-# LANGUAGE DeriveDataTypeable #-}

module Arpack.Exceptions where

import Control.Exception
import Data.Int (Int32)
import Data.Typeable

data MaxIterations = MaxIterations deriving (Show, Typeable)

instance Exception MaxIterations

data NoShifts = NoShifts deriving (Show, Typeable)

instance Exception NoShifts

data Reallocate = Reallocate deriving (Show, Typeable)

instance Exception Reallocate

data XYAUPD = XYAUPD Int32 deriving (Show, Typeable)

instance Exception XYAUPD

data XYEUPD = XYEUPD Int32 deriving (Show, Typeable)

instance Exception XYEUPD

data Unimplemented = Unimplemented Int32 deriving (Show, Typeable)

instance Exception Unimplemented
