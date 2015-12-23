{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Arpack.Options where

import Data.Complex

data Which :: * -> * where
  LM :: Which a
  SM :: Which a
  LR :: Which a
  SR :: Which a
  LI :: Which (Complex a)
  SI :: Which (Complex a)

showWhich :: Which a -> String
showWhich LM = "LM"
showWhich SM = "SM"
showWhich LR = "LR"
showWhich SR = "SR"
showWhich LI = "LI"
showWhich SI = "SI"

data Options a = Options { which :: !(Which a)
                         , number :: {-# UNPACK #-} !Int
                         , maxIterations :: !(Maybe Int)
                         , findVectors :: !Bool
                         }
