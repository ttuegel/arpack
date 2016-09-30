module Main where

import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (IOVector)
import Numeric.LinearAlgebra hiding (eig, vector)
import Numeric.LinearAlgebra.Arnoldi
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Arnoldi.eig" $ do

    it "returns the diagonal of an arbitrary diagonal matrix" $ property $ do
      dim <- arbitrary `suchThat` (> 3)
      diagonal <- VS.fromList <$> vector dim
      let
        _matrix = diag diagonal :: Matrix Double
        nev = dim - 3
        options = Options { which = SR
                          , number = nev
                          , maxIterations = Nothing
                          }
        sort = VS.modify Intro.sort
        actual = (sort . fst) (eig options dim (multiply _matrix))
        expected = (VS.take nev . sort) diagonal
        relative = VS.maximum (VS.zipWith (%) expected actual)
      pure (counterexample (show (expected, actual)) (relative < 1E-4))

    it "returns the diagonal of an arbitrary diagonal matrix" $ property $ do
      dim <- arbitrary `suchThat` (> 3)
      diagonal <- VS.fromList <$> vector dim
      let
        _matrix = diag diagonal :: Matrix (Complex Double)
        nev = dim - 3
        options = Options { which = SR
                          , number = nev
                          , maxIterations = Nothing
                          }
        sort = VS.modify (Intro.sortBy (comparing realPart))
        actual = (sort . fst) (eig options dim (multiply _matrix))
        expected = (VS.take nev . sort) diagonal
        relative = VS.maximum (VS.zipWith (%%) expected actual)
      pure (counterexample (show (expected, actual)) (relative < 1E-4))

multiply :: Numeric a => Matrix a -> IOVector a -> IOVector a -> IO ()
multiply _matrix dst src = do
  x <- VS.freeze src
  let y = _matrix #> x
  VS.copy dst y

(%) :: Double -> Double -> Double
(%) a b =
  let a_plus_b = a + b
  in if a_plus_b /= 0
     then abs ((a - b) / a_plus_b)
     else a - b

(%%) :: Complex Double -> Complex Double -> Double
(%%) a b =
  let a_plus_b = a + b
  in if a_plus_b /= 0
     then magnitude ((a - b) / a_plus_b)
     else magnitude (a - b)
