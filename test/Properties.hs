{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Properties where

import Data.AEq
import Data.Complex
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Packed as HMatrix
import qualified Data.Vector.Unboxed as Unboxed
import Numeric.Arpack
import qualified Numeric.LinearAlgebra as HMatrix
import Test.Tasty.QuickCheck as QC
import Test.Tasty

type Mat t = Unboxed.Vector (Int, Int, t)

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
           $ Unboxed.map (\(i, j, x) -> (i, j, x :+ 0)) mat

zeig :: Mat (Complex Double) -> [Complex Double]
zeig mat = take (dim mat - 2)
           $ sortBy (comparing realPart)
           $ HMatrix.toList
           $ HMatrix.eigenvalues
           $ densify mat

deigs :: Mat Double -> [Double]
deigs mat = Unboxed.toList $ eigenvalues (opts mat) mat (dim mat)

zeigs :: Mat (Complex Double) -> [Complex Double]
zeigs mat = Unboxed.toList $ eigenvalues (opts mat) mat (dim mat)

opts :: Unboxed.Unbox t => Unboxed.Vector (Int, Int, t) -> Options t
opts mat = Options { which = (Smallest, Real)
                   , number = dim mat - 2
                   , maxIterations = Nothing
                   }

densify :: (HMatrix.Element t, Unboxed.Unbox t) => Mat t -> HMatrix.Matrix t
densify mat = HMatrix.fromRows $ map selectRow [0..(dim mat - 1)]
  where
    selectRow i = HMatrix.fromList
                  $ Unboxed.toList
                  $ Unboxed.map (\(_, _, x) -> x)
                  $ Unboxed.filter (\(j, _, _) -> i == j) mat

dim :: (Unboxed.Unbox t) => Unboxed.Vector (Int, Int, t) -> Int
dim mat = let (_, cols, _) = Unboxed.unzip3 mat in (Unboxed.maximum cols) + 1

instance (QC.Arbitrary t, Unboxed.Unbox t) => QC.Arbitrary (Unboxed.Vector (Int, Int, t)) where
  arbitrary = do
    n <- fmap abs arbitrarySizedIntegral `suchThat` (> 2)
    xs <- fmap Unboxed.fromList $ vector (n * n)
    let ixs = Unboxed.fromList $ sort [(i, j) | i <- [0..(n - 1)], j <- [0..(n - 1)]]
    return $ Unboxed.zipWith (\(i, j) x -> (i, j, x)) ixs xs
