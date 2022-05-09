module Grid ( Array2
            , Ix2(..)) where


import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Massiv.Array as A

type Array2 a = A.Array A.U A.Ix2 a


-- printing
-- sensible imports