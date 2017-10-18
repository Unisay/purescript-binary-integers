module Data.Binary.UnsignedInt
  ( UnsignedInt
  , fromInt
  ) where

import Data.Array as A
import Data.Binary (class Binary, class FitsInt, class Fixed, Bit, numBits)
import Data.Binary as Bin
import Data.BooleanAlgebra ((&&))
import Data.Eq (class Eq, eq)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing, Just), fromMaybe')
import Data.Ord ((<=))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Typelevel.Num (class GtEq, class Lt, D32)
import Data.Typelevel.Num as Nat
import Data.Typelevel.Num.Aliases (D31)
import Data.Typelevel.Num.Sets (class Pos)
import Data.Typelevel.Undefined (undefined)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))


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

instance binaryUnsignedInt :: Pos b => Binary (UnsignedInt b) where
  invert (UnsignedInt b bs) = UnsignedInt b (Bin.invert bs)
  add' bit (UnsignedInt b as) (UnsignedInt _ bs) = UnsignedInt b <$> Bin.add' bit as bs
  zero = UnsignedInt undefined Bin.zero
  leftShift bit (UnsignedInt b bs) = UnsignedInt b <$> Bin.leftShift bit bs
  rightShift bit (UnsignedInt b bs) = UnsignedInt b <$> Bin.rightShift bit bs
  toBits (UnsignedInt b bs) = bs

instance fixedUnsignedInt :: Pos b => Fixed (UnsignedInt b) where
  numBits _ = Nat.toInt (undefined :: b)
  tryFromBits bits =
    if A.length bits <= numBits p
    then Just (UnsignedInt undefined bits)
    else Nothing
    where
      p :: Proxy (UnsignedInt b)
      p = Proxy

instance fitsIntUnsignedInt :: (Pos b, Lt b D32) => FitsInt (UnsignedInt b) where
  toInt ui@(UnsignedInt b bits) =
    -- Safe "by construction"
    fromMaybe' (\_ -> unsafeCrashWith err) (Bin.tryToInt bits)
      where err = "Failed to convert " <> show ui <> " to Int"
