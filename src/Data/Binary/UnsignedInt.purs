module Data.Binary.UnsignedInt
  ( UnsignedInt
  , fromInt
  ) where

import Prelude

import Data.Array as A
import Data.Binary (class Binary, class FitsInt, class Fixed, Bits(Bits), _0, _1, and, diffFixed, modAdd, modMul, numBits, or, xor)
import Data.Binary as Bin
import Data.Maybe (Maybe(Nothing, Just), fromMaybe')
import Data.Typelevel.Num (class Pos, class GtEq, class Lt, type (:*), D1, D16, D2, D31, D32, D5, D6, D64, D8)
import Data.Typelevel.Num as Nat
import Data.Typelevel.Undefined (undefined)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

type Uint8   = UnsignedInt D8
type Uint16  = UnsignedInt D16
type Uint32  = UnsignedInt D32
type Uint64  = UnsignedInt D64
type Uint128 = UnsignedInt (D1 :* D2 :* D8)
type Uint256 = UnsignedInt (D2 :* D5 :* D6)

data UnsignedInt b = UnsignedInt b Bits

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

instance ordUnsignedInt :: Pos b => Ord (UnsignedInt b) where
  compare (UnsignedInt n as) (UnsignedInt _ bs) = compare as bs

instance binaryUnsignedInt :: Pos b => Binary (UnsignedInt b) where
  _0 = UnsignedInt undefined _0
  _1 = UnsignedInt undefined _1
  and (UnsignedInt b as) (UnsignedInt _ bs) = UnsignedInt b (and as bs)
  xor (UnsignedInt b as) (UnsignedInt _ bs) = UnsignedInt b (xor as bs)
  or  (UnsignedInt b as) (UnsignedInt _ bs) = UnsignedInt b (or as bs)
  invert (UnsignedInt b bs) = UnsignedInt b (Bin.invert bs)
  add' bit (UnsignedInt b as) (UnsignedInt _ bs) = UnsignedInt b <$> Bin.add' bit as bs
  leftShift bit (UnsignedInt b bs) = UnsignedInt b <$> Bin.leftShift bit bs
  rightShift bit (UnsignedInt b bs) = UnsignedInt b <$> Bin.rightShift bit bs
  toBits (UnsignedInt b bs) = Bin.addLeadingZeros (Nat.toInt b) bs

instance boundedUnsignedInt :: Pos b => Bounded (UnsignedInt b) where
  bottom = _0
  top = UnsignedInt undefined (Bits (A.replicate (Nat.toInt (undefined :: b)) _1))

instance fixedUnsignedInt :: Pos b => Fixed (UnsignedInt b) where
  numBits _ = Nat.toInt (undefined :: b)
  tryFromBits (Bits bits) =
    if A.length stripped <= numBits p
    then Just (UnsignedInt undefined (Bits stripped))
    else Nothing
    where
      stripped = A.dropWhile (eq _0) bits
      p :: Proxy (UnsignedInt b)
      p = Proxy

instance fitsIntUnsignedInt :: (Pos b, Lt b D32) => FitsInt (UnsignedInt b) where
  toInt ui@(UnsignedInt b bits) =
    -- Safe "by construction"
    fromMaybe' (\_ -> unsafeCrashWith err) (Bin.tryToInt bits)
      where err = "Failed to convert " <> show ui <> " to Int"

instance semiringUnsignedInt :: Pos b => Semiring (UnsignedInt b) where
  zero = _0
  add = modAdd
  one = _1
  mul = modMul

instance ringUnsignedInt :: Pos b => Ring (UnsignedInt b) where
  sub = diffFixed
