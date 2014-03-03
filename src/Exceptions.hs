{-# LANGUAGE DeriveDataTypeable #-}
module Exceptions where

import Control.Exception
import Data.Typeable

data MaxIterations = MaxIterations deriving (Show, Typeable)

instance Exception MaxIterations

data NoShifts = NoShifts deriving (Show, Typeable)

instance Exception NoShifts
