module Data.Binary.SignedInt
  ( SignedInt
  , fromInt
  , isNegative
  ) where

import Prelude

import Data.Array (head)
import Data.Array as A
import Data.Binary (class Binary, class FitsInt, class Fixed, Bit(..), Bits(Bits), _0, _1, diffFixed, modAdd, modMul, numBits)
import Data.Binary as Bin
import Data.Maybe (Maybe(Nothing, Just), fromMaybe')
import Data.Ord (abs)
import Data.Typelevel.Num (class GtEq, class Lt, type (:*), D1, D16, D2, D32, D5, D6, D64, D8)
import Data.Typelevel.Num as Nat
import Data.Typelevel.Num.Sets (class Pos)
import Data.Typelevel.Undefined (undefined)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

type Int8   = SignedInt D8
type Int16  = SignedInt D16
type Int32  = SignedInt D32
type Int64  = SignedInt D64
type Int128 = SignedInt (D1 :* D2 :* D8)
type Int256 = SignedInt (D2 :* D5 :* D6)

data SignedInt b = SignedInt b Bits

instance eqSignedInt :: Pos b => Eq (SignedInt b) where
  eq (SignedInt n bits) (SignedInt n' bits') =
    eq (Nat.toInt n) (Nat.toInt n') && eq bits bits'

instance showSignedInt :: Pos b => Show (SignedInt b) where
  show (SignedInt n bits) =
    "SignedInt" <> show (Nat.toInt n) <> "#" <> Bin.toBinString bits

-- | `Int` = Width
-- | `Boolean` = is negative
twosComplement :: Int -> Boolean -> Bits -> Bits
twosComplement width false bits = Bin.addLeadingZeros width bits
twosComplement width true bits@(Bits bs) =
  case compare (A.length bs) (width - 1) of
  GT -> bits
  EQ -> complement bits
  LT -> complement (Bin.addLeadingZeros (width - 1) bits)
  where complement = Bin.invert >>> Bin.unsafeAdd _1 >>> append _1

-- | Converts `Int` value to `SignedInt b` for b >= 31
fromInt :: ∀ b . Pos b => GtEq b D32 => b -> Int -> SignedInt b
fromInt b i = SignedInt b signed where
  signed = twosComplement width (i < 0) bits
  width = Nat.toInt b
  bits = Bin.fromInt (abs i)

isNegative :: ∀ b . Pos b => SignedInt b -> Boolean
isNegative (SignedInt _ (Bits bits)) = head bits == Just (Bit true)

instance ordSignedInt :: Pos b => Ord (SignedInt b) where
  compare a b | isNegative a && not (isNegative b) = LT
  compare a b | not (isNegative a) && isNegative b = GT
  compare (SignedInt _ a) (SignedInt _ b) = compare a b








instance binarySignedInt :: Pos b => Binary (SignedInt b) where
  _0 = SignedInt undefined _0
  _1 = SignedInt undefined _1
  invert (SignedInt b bs) = SignedInt b (Bin.invert bs)
  add' bit (SignedInt b as) (SignedInt _ bs) = SignedInt b <$> Bin.add' bit as bs
  leftShift bit (SignedInt b bs) = SignedInt b <$> Bin.leftShift bit bs
  rightShift bit (SignedInt b bs) = SignedInt b <$> Bin.rightShift bit bs
  toBits (SignedInt b bs) = Bin.addLeadingZeros (Nat.toInt b) bs

instance boundedSignedInt :: Pos b => Bounded (SignedInt b) where
  bottom = _0
  top = SignedInt undefined (Bits (A.replicate (Nat.toInt (undefined :: b)) _1))

instance fixedSignedInt :: Pos b => Fixed (SignedInt b) where
  numBits _ = Nat.toInt (undefined :: b)
  tryFromBits (Bits bits) =
    if A.length stripped <= numBits p
    then Just (SignedInt undefined (Bits stripped))
    else Nothing
    where
      stripped = A.dropWhile (eq _0) bits
      p :: Proxy (SignedInt b)
      p = Proxy

instance fitsIntSignedInt :: (Pos b, Lt b D32) => FitsInt (SignedInt b) where
  toInt ui@(SignedInt b bits) =
    -- Safe "by construction"
    fromMaybe' (\_ -> unsafeCrashWith err) (Bin.tryToInt bits)
      where err = "Failed to convert " <> show ui <> " to Int"

instance semiringSignedInt :: Pos b => Semiring (SignedInt b) where
  zero = _0
  add = modAdd
  one = _1
  mul = modMul

instance ringSignedInt :: Pos b => Ring (SignedInt b) where
  sub = diffFixed
