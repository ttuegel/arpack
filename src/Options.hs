{-# LANGUAGE GADTs #-}
module Options where

import Data.Complex
import Data.Function (on)
import Data.Ord

data Comparison = Largest | Smallest

data Component a where
  Magnitude :: Component a
  Real :: Component a
  Imaginary :: Component (Complex a)

data Options a = Options { which :: (Comparison, Component a)
                         , number :: Int
                         , maxIterations :: Maybe Int
                         }

whichString :: Options t -> String
whichString opts =
  let (h, w) = which opts
      c = case h of
            Largest -> 'L'
            Smallest -> 'S'
      d = case w of
            Magnitude -> 'M'
            Real -> 'R'
            Imaginary -> 'I'
  in c : d : []

compareReal :: (Num t, Ord t) => (Comparison, Component t) -> t -> t -> Ordering
compareReal (how, _which) =
  let f = case _which of
            Magnitude -> abs
            Real -> id
      c = case how of
            Smallest -> compare
            Largest -> comparing Down
  in c `on` f

compareComplex :: (Num t, Ord t, RealFloat t)
               => (Comparison, Component (Complex t)) -> Complex t -> Complex t -> Ordering
compareComplex (how, _which) =
  let f = case _which of
            Magnitude -> magnitude
            Real -> realPart
            Imaginary -> imagPart
      c = case how of
            Smallest -> compare
            Largest -> comparing Down
  in c `on` f
