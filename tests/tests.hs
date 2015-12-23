module Main where

import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (IOVector, Storable)
import Numeric.LinearAlgebra hiding (eig, vector)
import Numeric.LinearAlgebra.Arnoldi
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Arnoldi.eig" $ do

    it "returns the diagonal of an arbitrary diagonal matrix" $ property $ do
      dim <- arbitrary `suchThat` (> 3)
      diagonal <- VS.modify Intro.sort . VS.fromList <$> vector dim
      let
        _matrix = diag diagonal :: Matrix Double
        nev = dim - 3
        options = Options { which = SR
                          , number = nev
                          , maxIterations = Nothing
                          }
        (evals, _) = eig options dim (multiply _matrix)
      pure (evals == VS.take nev diagonal)

multiply :: (Numeric a, Storable a)
         => Matrix a -> IOVector a -> IOVector a -> IO ()
multiply _matrix dst src = do
  x <- VS.freeze src
  let y = _matrix #> x
  VS.copy dst y
