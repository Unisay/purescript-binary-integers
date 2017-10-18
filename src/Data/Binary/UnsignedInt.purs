module Data.Binary.UnsignedInt
  ( UnsignedInt
  , fromInt
  , toInt
  , tryFromInt
  , toBinString
  ) where

import Data.Array as A
import Data.Binary as Bin
import Data.Typelevel.Num as Nat
import Data.Binary (Bit)
import Data.BooleanAlgebra ((&&))
import Data.Eq (class Eq, eq)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe')
import Data.Ord ((<), (<=))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Typelevel.Num (class GtEq, class Lt, D32)
import Data.Typelevel.Num.Aliases (D31)
import Data.Typelevel.Num.Sets (class Pos)
import Partial.Unsafe (unsafeCrashWith)


data UnsignedInt b = UnsignedInt b (Array Bit)

instance eqUnsignedInt :: Pos b =>
                          Eq (UnsignedInt b) where
  eq (UnsignedInt n bits) (UnsignedInt n' bits') =
    eq (Nat.toInt n) (Nat.toInt n') && eq bits bits'

instance showUnsignedInt :: Pos b => Show (UnsignedInt b) where
  show (UnsignedInt n bits) =
    "UnsignedInt" <> show (Nat.toInt n) <> "#" <> Bin.toBinString bits

-- | Converts `Int` value to `UnsignedInt b` for b >= 31
-- | Behavior for negative `Int` values is unspecified.
fromInt :: ∀ b . Pos b => GtEq b D31 => b -> Int -> UnsignedInt b
fromInt b i = UnsignedInt b (Bin.fromInt i)

toInt :: ∀ b . Pos b => Lt b D32 => UnsignedInt b -> Int
toInt ui@(UnsignedInt b bits) =
  -- Safe "by construction"
  fromMaybe' (\_ -> unsafeCrashWith err) (Bin.tryToInt bits)
    where err = "Failed to convert " <> show ui <> " to Int"

-- | Converts `Int` value to `UnsignedInt b` for b > 0
-- | Returns `Just` for non-negative `Int` values that fit in b bits.
-- | Returns `Nothing` otherwise.
tryFromInt :: ∀ b . Pos b => b -> Int -> Maybe (UnsignedInt b)
tryFromInt _ i | i < 0 = Nothing
tryFromInt b i =
  if A.length bits <= Nat.toInt b
  then Just (UnsignedInt b bits)
  else Nothing
  where bits = Bin.intToBitArray i

toBinString :: ∀ b . UnsignedInt b -> String
toBinString (UnsignedInt _ bits) = Bin.toBinString bits
