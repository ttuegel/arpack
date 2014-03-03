{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-} -- `unsafePerformIO`

module Numeric.Arpack
       ( Options(..), Comparison(..), Component(..)
       , eigensystem, eigenvalues
       ) where

import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Loop
import Data.Complex
import Data.Function (on)
import qualified Data.Vector.Algorithms.Heap as Heap
import qualified Data.Vector.Generic as Vec
import qualified Data.Vector.Generic.Mutable as Mut
import qualified Data.Vector.Storable as Storable hiding (unsafeWith)
import qualified Data.Vector.Storable.Mutable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import Foreign hiding (new, unsafePerformIO, void)
import Foreign.C.String
import Foreign.Storable.Complex ()
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

import Internal
import Options

-- Interface ----------

{-# NOINLINE eigenvalues #-}
eigenvalues :: Arpack t => Options t -> Unboxed.Vector (Int, Int, t) -> Int -> Unboxed.Vector t
eigenvalues opts mat dim = unsafePerformIO $ do
  (unsortEvals, _) <- arpack False opts mat dim
  sorting <- Vec.unsafeThaw unsortEvals
  Heap.sortBy (compare_ $ which opts) sorting
  Vec.unsafeFreeze sorting

{-# NOINLINE eigensystem #-}
eigensystem :: Arpack t => Options t -> Unboxed.Vector (Int, Int, t) -> Int
            -> (Unboxed.Vector t, Unboxed.Vector t)
eigensystem opts mat dim = unsafePerformIO $ do
  (unsortEvals, unsortEvecs) <- arpack True opts mat dim
  sorting <- Vec.unsafeThaw $ Vec.indexed unsortEvals
  Heap.sortBy (compare_ (which opts) `on` snd) sorting
  (ordering, evals) <- liftM Vec.unzip $ Vec.unsafeFreeze sorting
  let permutation = Vec.concatMap (\i -> let off = i * dim
                                         in Vec.map (+ off) (Vec.enumFromN 0 dim))
                                  ordering
      evecs = Vec.unsafeBackpermute (Vec.convert unsortEvecs) permutation
  return (evals, evecs)

-- Arpack driver ----------

class Unboxed.Unbox t => Arpack t where
  arpack :: Bool -> Options t -> Unboxed.Vector (Int, Int, t) -> Int
         -> IO (Unboxed.Vector t, Unboxed.Vector t)
  compare_ :: (Comparison, Component t) -> t -> t -> Ordering

instance Arpack Float where
  arpack = arpackWrapper snaupd_ sneupd_
  compare_ = compareReal

instance Arpack Double where
  arpack = arpackWrapper dnaupd_ dneupd_
  compare_ = compareReal

instance Arpack (Complex Float) where
  arpack = arpackWrapper cnaupd_ cneupd_
  compare_ = compareComplex

instance Arpack (Complex Double) where
  arpack = arpackWrapper znaupd_ zneupd_
  compare_ = compareComplex

arpackWrapper :: (Num t, Num real, Storable t, Storable real, Unboxed.Unbox t)
              => XXaupd t real -> XXeupd t real
              -> Bool
              -> Options t
              -> Unboxed.Vector (Int, Int, t)
              -> Int
              -> IO (Unboxed.Vector t, Unboxed.Vector t)
arpackWrapper aupd eupd !findVectors !opts !mat !dim = withPool $ \pool ->
       -- These variables are all banged because we need to be strict
       -- in them _before_ we enter the locked segment of code! If we
       -- wait until we're inside the lock, and evaluating one of these
       -- variables invokes 'arpack' again, the program will deadlock!
  do let nev = number opts
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
     tol <- new 0
     _ncv <- new $ fromIntegral ncv
     v <- array (dim * ncv)
     ldv <- new $ fromIntegral dim
     _lworkl <- new $ fromIntegral lworkl
     aupdInfo <- new 0

     rvec <- new $ if findVectors then 1 else 0
     ldz <- new $ fromIntegral dim
     sigma <- new 0
     eupdInfo <- new 0

     withCString "I" $ \bmat ->
       withCString (whichString opts) $ \_which ->
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
              lift $ aupd ido bmat n _which _nev tol resid _ncv v
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
              eupd rvec howmny select d z ldz sigma workev bmat n
                   _which _nev tol resid _ncv v ldv iparam ipntr workd
                   workl _lworkl rwork eupdInfo

            do info <- peek eupdInfo
               case info of
                    0 -> return ()
                    _ -> printf "zneupd: info = %d" (fromIntegral info :: Int)

            return (evalsM, evecsM)

          evals <- Storable.freeze evalsM
          evecs <- Storable.freeze evecsM
          return (Vec.convert $ Vec.take nev evals, Vec.convert $ Vec.take (dim * nev) evecs)

-- Utilities ----------

{-# NOINLINE arpackLock #-}
arpackLock :: Lock
arpackLock = unsafePerformIO $ Lock.new

pokeOff :: Storable a => Ptr a -> Int -> a -> IO ()
pokeOff p off = poke (advancePtr p off)

peekOff :: Storable a => Ptr a -> Int -> IO a
peekOff p off = peek (advancePtr p off)

multiply :: (Num t, Storable t, Unboxed.Unbox t)
         => Int
         -> Unboxed.Vector (Int, Int, t)
         -> Ptr t
         -> Ptr t
         -> IO ()
multiply dim mat workX workY = do
  tmpX <- Mut.new dim
  Storable.unsafeWith tmpX $ \p -> copyArray p workX dim
  tmpY <- Mut.replicate dim 0
  Vec.forM_ mat $ \(r, c, a) -> do
    x <- Mut.unsafeRead tmpX c
    Mut.unsafeRead tmpY r >>= Mut.unsafeWrite tmpY r . (+ (a * x))
  Storable.unsafeWith tmpY $ \p -> copyArray workY p dim
