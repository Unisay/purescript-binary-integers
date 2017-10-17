module Data.Binary.UnsignedInt
  ( UnsignedInt
  ) where

import Data.Typelevel.Num as N
import Data.Binary (Bit)
import Data.BooleanAlgebra ((&&))
import Data.Eq (class Eq, eq)
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Typelevel.Num (class GtEq)
import Data.Typelevel.Num.Aliases (D31)
import Data.Typelevel.Num.Sets (class Pos)

data UnsignedInt n = UnsignedInt n (Array Bit)

instance eqUnsignedInt :: Pos n => Eq (UnsignedInt n) where
  eq (UnsignedInt n bits) (UnsignedInt n' bits') = eq (N.toInt n) (N.toInt n') && eq bits bits'

instance showUnsignedInt :: Pos n => Show (UnsignedInt n) where
  show (UnsignedInt n bits) = "UnsignedInt " <> show (N.toInt n) <> "#" <> show bits

-- fromInt :: ∀ n . GtEq n D31 => Int -> Maybe (UnsignedInt n)
-- fromInt _ = ?fromInt
