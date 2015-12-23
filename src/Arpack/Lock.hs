{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-} -- `unsafePerformIO`

module Arpack.Lock ( lock ) where

import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import System.IO.Unsafe (unsafePerformIO)

lock :: Lock
{-# NOINLINE lock #-}
lock = unsafePerformIO $ Lock.new
