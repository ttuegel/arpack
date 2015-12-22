{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Properties where

import Data.AEq
import Data.Complex
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as VU
import qualified Numeric.LinearAlgebra as HMatrix
import Test.Tasty.QuickCheck as QC
import Test.Tasty

import Numeric.LinearAlgebra.Arnoldi

type Mat t = VU.Vector (Int, Int, t)

properties =
  testGroup "QuickCheck properties"
  [ QC.testProperty "sparse vs. dense eigenvalues :: Double"
    $ \mat -> all ((< 1.0E-8) . abs) $ zipWith (-) (deigs mat) (deig mat)
  , QC.testProperty "sparse vs. dense eigenvalues :: Complex Double"
    $ \mat -> all ((< 1.0E-8) . magnitude) $ zipWith (-) (zeigs mat) (zeig mat)
  ]

deig :: Mat Double -> [Double]
deig mat = map realPart
           $ zeig
           $ VU.map (\(i, j, x) -> (i, j, x :+ 0)) mat

zeig :: Mat (Complex Double) -> [Complex Double]
zeig mat = take (dim mat - 2)
           $ sortBy (comparing realPart)
           $ HMatrix.toList
           $ HMatrix.eigenvalues
           $ densify mat

deigs :: Mat Double -> [Double]
deigs mat = VU.toList $ eigenvalues (opts mat) mat (dim mat)

zeigs :: Mat (Complex Double) -> [Complex Double]
zeigs mat = VU.toList $ eigenvalues (opts mat) mat (dim mat)

opts :: VU.Unbox t => VU.Vector (Int, Int, t) -> Options t
opts mat = Options { which = (Smallest, Real)
                   , number = dim mat - 2
                   , maxIterations = Nothing
                   }

densify :: (HMatrix.Element t, VU.Unbox t) => Mat t -> HMatrix.Matrix t
densify mat = HMatrix.fromRows $ map selectRow [0..(dim mat - 1)]
  where
    selectRow i = HMatrix.fromList
                  $ VU.toList
                  $ VU.map (\(_, _, x) -> x)
                  $ VU.filter (\(j, _, _) -> i == j) mat

dim :: (VU.Unbox t) => VU.Vector (Int, Int, t) -> Int
dim mat = let (_, cols, _) = VU.unzip3 mat in (VU.maximum cols) + 1

instance (QC.Arbitrary t, VU.Unbox t) => QC.Arbitrary (VU.Vector (Int, Int, t)) where
  arbitrary = do
    n <- fmap abs arbitrarySizedIntegral `suchThat` (> 2)
    xs <- fmap VU.fromList $ vector (n * n)
    let ixs = VU.fromList $ sort [(i, j) | i <- [0..(n - 1)], j <- [0..(n - 1)]]
    return $ VU.zipWith (\(i, j) x -> (i, j, x)) ixs xs
