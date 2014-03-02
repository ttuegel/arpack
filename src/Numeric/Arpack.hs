{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-} -- `unsafePerformIO`

module Numeric.Arpack where

import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Loop
import Data.Complex
import qualified Data.Vector.Generic as Vec
import qualified Data.Vector.Generic.Mutable as Mut
import qualified Data.Vector.Storable as Storable (Vector)
import qualified Data.Vector.Storable.Mutable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import Foreign hiding (new, unsafePerformIO, void)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable.Complex ()
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

-- Foreign imports ----------

foreign import ccall unsafe "znaupd_" znaupd_
  :: Ptr CInt             -- ido
  -> CString              -- bmat
  -> Ptr CInt             -- n
  -> CString              -- which
  -> Ptr CInt             -- nev
  -> Ptr Double           -- tol
  -> Ptr (Complex Double) -- resid
  -> Ptr CInt             -- ncv
  -> Ptr (Complex Double) -- v
  -> Ptr CInt             -- ldv
  -> Ptr CInt             -- iparam
  -> Ptr CInt             -- ipntr
  -> Ptr (Complex Double) -- workd
  -> Ptr (Complex Double) -- workl
  -> Ptr CInt             -- lworkl
  -> Ptr Double           -- rwork
  -> Ptr CInt             -- info
  -> IO ()

foreign import ccall unsafe "zneupd_" zneupd_
  :: Ptr CInt             -- rvec
  -> CString              -- all
  -> Ptr CInt             -- select
  -> Ptr (Complex Double) -- d
  -> Ptr (Complex Double) -- z
  -> Ptr CInt             -- ldz
  -> Ptr Double           -- sigma
  -> Ptr (Complex Double) -- workev
  -> CString              -- bmat
  -> Ptr CInt             -- n
  -> CString              -- which
  -> Ptr CInt             -- nev
  -> Ptr Double           -- tol
  -> Ptr (Complex Double) -- resid
  -> Ptr CInt             -- ncv
  -> Ptr (Complex Double) -- v
  -> Ptr CInt             -- ldv
  -> Ptr CInt             -- iparam
  -> Ptr CInt             -- ipntr
  -> Ptr (Complex Double) -- workd
  -> Ptr (Complex Double) -- workl
  -> Ptr CInt             -- lworkl
  -> Ptr Double           -- rwork
  -> Ptr CInt             -- ierr
  -> IO ()

-- Arpack driver ----------

arpack :: Bool -- ^ return eigenvectors?
       -> Unboxed.Vector (Int, Int, Complex Double) -- ^ sparse matrix
       -> Int -- ^ matrix dimension
       -> Int -- ^ desired number of eigenvalues to return
       -> IO ( Storable.Vector (Complex Double) -- ^ eigenvalues
             , Storable.Vector (Complex Double) -- ^ eigenvectors
             )
arpack !findVectors !mat !dim !nev = withPool $ \pool ->
       -- These variables are all banged because we need to be strict
       -- in them _before_ we enter the locked segment of code! If we
       -- wait until we're inside the lock, and evaluating one of these
       -- variables invokes 'arpack' again, the program will deadlock!
  do let
         -- Largest number of basis vectors to use.
         -- Work per iteration is O(dim * ncv ^ 2).
         ncv = min dim (4 * nev)
         lworkl = 3 * ncv * ncv + 5 * ncv
         new :: Storable a => a -> IO (Ptr a)
         new = pooledNew pool
         array :: Storable a => Int -> IO (Ptr a)
         array = pooledMallocArray pool

     select <- array ncv
     workev <- array (3 * ncv)
     resid <- array dim
     iparam <- array 11
     ipntr <- array 14
     workd <- array (3 * dim)
     workl <- array lworkl
     rwork <- array ncv

     ido <- new 0
     n <- new $ fromIntegral dim
     _nev <- new $ fromIntegral nev
     tol <- new 0.0
     _ncv <- new $ fromIntegral ncv
     v <- array (dim * ncv)
     ldv <- new $ fromIntegral dim
     _lworkl <- new $ fromIntegral lworkl
     aupdInfo <- new 0

     rvec <- new $ if findVectors then 1 else 0
     ldz <- new $ fromIntegral dim
     sigma <- new 0.0
     eupdInfo <- new 0

     withCString "I" $ \bmat ->
       withCString "SR" $ \which ->
       withCString "A" $ \howmny ->

       do -- Shift strategy (1 -> exact)
          pokeOff iparam 0 1
          -- Maximum number of iterations
          pokeOff iparam 2 (fromIntegral $ 3 * dim)
          -- Mode of znaupd
          -- 1 -> exact shift, 2 -> user-supplied shift, 3 -> shift-invert
          -- 4 -> buckling, 5 -> Cayley
          pokeOff iparam 6 1

          -- The big lock used to be around the entire function body, but
          -- it's better to lock as little code as possible!
          (evalsM, evecsM) <- Lock.with arpackLock $ do
            repeatLoopT $ do
              lift $ znaupd_ ido bmat n which _nev tol resid _ncv v
                             ldv iparam ipntr workd workl _lworkl rwork aupdInfo
              i <- lift $ peek ido
              case i of
                99 -> exit
                _ | i == 1 || i == (-1) -> lift $ do
                      offX <- liftM (pred . fromIntegral) $ peekOff ipntr 0
                      offY <- liftM (pred . fromIntegral) $ peekOff ipntr 1
                      let x = advancePtr workd offX
                          y = advancePtr workd offY
                      multiply dim mat x y
                  | otherwise -> error "arpack: the impossible happened!"

            do info <- peek aupdInfo
               case info of
                    0 -> return ()
                    _ -> printf "znaupd: info = %d" (fromIntegral info :: Int)

            evalsM <- Mut.new (nev + 1)
            evecsM <- Mut.new (dim * nev)
            Storable.unsafeWith evalsM $ \d ->
              Storable.unsafeWith evecsM $ \z ->
              zneupd_ rvec howmny select d z ldz sigma workev bmat n
                      which _nev tol resid _ncv v ldv iparam ipntr workd
                      workl _lworkl rwork eupdInfo

            do info <- peek eupdInfo
               case info of
                    0 -> return ()
                    _ -> printf "zneupd: info = %d" (fromIntegral info :: Int)

            return (evalsM, evecsM)

          evals <- Vec.freeze evalsM
          evecs <- Vec.freeze evecsM
          return (Vec.take nev evals, Vec.take (dim * nev) evecs)

-- Utilities ----------

{-# NOINLINE arpackLock #-}
arpackLock :: Lock
arpackLock = unsafePerformIO $ Lock.new

pokeOff :: Storable a => Ptr a -> Int -> a -> IO ()
pokeOff p off = poke (advancePtr p off)

peekOff :: Storable a => Ptr a -> Int -> IO a
peekOff p off = peek (advancePtr p off)

multiply :: Int
         -> Unboxed.Vector (Int, Int, Complex Double)
         -> Ptr (Complex Double)
         -> Ptr (Complex Double)
         -> IO ()
multiply dim mat workX workY = do
  tmpX <- Mut.new dim
  Storable.unsafeWith tmpX $ \p -> copyArray p workX dim
  tmpY <- Mut.replicate dim (0.0 :+ 0.0)
  Vec.forM_ mat $ \(r, c, a) -> do
    x <- Mut.unsafeRead tmpX c
    Mut.unsafeRead tmpY r >>= Mut.unsafeWrite tmpY r . (+ (a * x))
  Storable.unsafeWith tmpY $ \p -> copyArray workY p dim
