module Data.Binary.UnsignedInt
  ( UnsignedInt
  , fromInt
  ) where

import Prelude

import Data.Array as A
import Data.Binary (class Binary, class FitsInt, class Fixed, Bits(Bits), _0, _1, and, diffFixed, modAdd, modMul, numBits, or, toStringAs, xor)
import Data.Binary as Bin
import Data.Binary.BaseN (class BaseN)
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

newtype UnsignedInt b = UnsignedInt Bits

instance eqUnsignedInt :: Pos b => Eq (UnsignedInt b) where
  eq (UnsignedInt bits) (UnsignedInt bits') = eq bits bits'

instance ordUnsignedInt :: Pos b => Ord (UnsignedInt b) where
  compare (UnsignedInt as) (UnsignedInt bs) = compare as bs

instance showUnsignedInt :: Pos b => Show (UnsignedInt b) where
  show (UnsignedInt bits) =
    "UnsignedInt" <> show (Nat.toInt (undefined :: b)) <> "#" <> Bin.toBinString bits

-- | Converts `Int` value to `UnsignedInt b` for b >= 31
-- | Behavior for negative `Int` values is unspecified.
fromInt :: ∀ b . Pos b => GtEq b D31 => b -> Int -> UnsignedInt b
fromInt b i = UnsignedInt (Bin.fromInt i)

instance binaryUnsignedInt :: Pos b => Binary (UnsignedInt b) where
  _0 = UnsignedInt _0
  _1 = UnsignedInt _1
  and (UnsignedInt as) (UnsignedInt bs) = UnsignedInt (and as bs)
  xor (UnsignedInt as) (UnsignedInt bs) = UnsignedInt (xor as bs)
  or  (UnsignedInt as) (UnsignedInt bs) = UnsignedInt (or as bs)
  invert (UnsignedInt bs) = UnsignedInt (Bin.invert bs)
  add' bit (UnsignedInt as) (UnsignedInt bs) = UnsignedInt <$> Bin.add' bit as bs
  leftShift bit (UnsignedInt bs) = UnsignedInt <$> Bin.leftShift bit bs
  rightShift bit (UnsignedInt bs) = UnsignedInt <$> Bin.rightShift bit bs
  toBits (UnsignedInt bs) = Bin.addLeadingZeros (Nat.toInt (undefined :: b)) bs

instance boundedUnsignedInt :: Pos b => Bounded (UnsignedInt b) where
  bottom = _0
  top = UnsignedInt (Bits (A.replicate (Nat.toInt (undefined :: b)) _1))

instance fixedUnsignedInt :: Pos b => Fixed (UnsignedInt b) where
  numBits _ = Nat.toInt (undefined :: b)
  tryFromBits (Bits bits) =
    if A.length stripped <= numBits p
    then Just (UnsignedInt (Bits stripped))
    else Nothing
    where
      stripped = A.dropWhile (eq _0) bits
      p :: Proxy (UnsignedInt b)
      p = Proxy

instance fitsIntUnsignedInt :: (Pos b, Lt b D32) => FitsInt (UnsignedInt b) where
  toInt ui@(UnsignedInt bits) =
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

instance baseNUnsignedInt :: Pos b => BaseN (UnsignedInt b) where
  toBase r (UnsignedInt bits) = toStringAs r bits
