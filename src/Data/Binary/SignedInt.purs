module Data.Binary.SignedInt
  ( SignedInt
  , fromInt
  , isNegative
  , complement
  ) where

import Prelude

import Data.Array as A
import Data.Binary (class Binary, class FitsInt, class Fixed, Bits(Bits), Overflow(Overflow), _0, _1, and, diffFixed, head, modAdd, modMul, msb, numBits, or, tail, toStringAs, xor)
import Data.Binary as Bin
import Data.Binary.BaseN (class BaseN, toBase)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, fromMaybe')
import Data.Ord (abs)
import Data.Typelevel.Num (class GtEq, class LtEq, type (:*), D1, D16, D2, D32, D5, D6, D64, D8)
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

complement :: ∀ b. Pos b => SignedInt b -> SignedInt b
complement si = (Bin.invert >>> Bin.unsafeAdd _1) si

-- | Converts `Int` value to `SignedInt b` for b >= 31
fromInt :: ∀ b . Pos b => GtEq b D32 => b -> Int -> SignedInt b
fromInt b i = SignedInt b signed where
  signed = twosComplement (Nat.toInt b) (i < 0) (Bin.fromInt (abs i))
  twosComplement :: Int -> Boolean -> Bits -> Bits
  twosComplement w false bits = Bin.addLeadingZeros w bits
  twosComplement w true bits@(Bits bs) =
    case compare (A.length bs) (w - 1) of
    GT -> bits
    EQ -> compl bits
    LT -> compl (Bin.addLeadingZeros (w - 1) bits)
  compl = Bin.invert >>> Bin.unsafeAdd _1 >>> append _1

isNegative :: ∀ b . SignedInt b -> Boolean
isNegative (SignedInt _ bits) = head bits == _1

instance ordSignedInt :: Pos b => Ord (SignedInt b) where
  compare a b | isNegative a && not (isNegative b) = LT
  compare a b | not (isNegative a) && isNegative b = GT
  compare (SignedInt _ a) (SignedInt _ b) = compare a b

instance binarySignedInt :: Pos b => Binary (SignedInt b) where
  _0 = SignedInt undefined _0
  _1 = SignedInt undefined _1
  and (SignedInt b as) (SignedInt _ bs) = SignedInt b (and as bs)
  xor (SignedInt b as) (SignedInt _ bs) = SignedInt b (xor as bs)
  or (SignedInt b as) (SignedInt _ bs) = SignedInt b (or as bs)
  invert (SignedInt b bs) = SignedInt b (Bin.invert bs)
  add' bit (SignedInt b as) (SignedInt _ bs) =
    let (Overflow o xs) = Bin.add' bit as bs
    in Overflow (xor o (msb xs)) (SignedInt b xs)
  leftShift bit (SignedInt b bs) = SignedInt b <$> Bin.leftShift bit bs
  rightShift bit (SignedInt b bs) = SignedInt b <$> Bin.rightShift bit bs
  toBits (SignedInt _ bs) = bs

instance boundedSignedInt :: Pos b => Bounded (SignedInt b) where
  bottom = SignedInt undefined (Bits (A.cons _1 (A.replicate (Nat.toInt (undefined :: b)) _0)))
  top    = SignedInt undefined (Bits (A.cons _0 (A.replicate (Nat.toInt (undefined :: b)) _1)))

instance fixedSignedInt :: Pos b => Fixed (SignedInt b) where
  numBits _ = Nat.toInt (undefined :: b)
  tryFromBits (Bits bits) = tryFromBits' (A.length bits) (numBits p) bits where
    tryFromBits' len width bs | len == width = Just (SignedInt undefined (Bits bs))
    tryFromBits' len width bs | len < width = Just (SignedInt undefined (Bin.addLeadingZeros width (Bits bs)))
    tryFromBits' _ _ _ = Nothing
    p :: Proxy (SignedInt b)
    p = Proxy

instance fitsIntSignedInt :: (Pos b, LtEq b D32) => FitsInt (SignedInt b) where
  toInt si@(SignedInt _ bits) | (head bits) == _1 = negate (Bin.toInt (complement si))
  toInt si@(SignedInt _ bits) = abs where
    -- Safe "by construction"
    abs = fromMaybe' (\_ -> unsafeCrashWith err) (Bin.tryToInt (tail bits))
    err = "Failed to convert " <> show si <> " to Int"

instance semiringSignedInt :: Pos b => Semiring (SignedInt b) where
  zero = _0
  add = modAdd
  one = _1
  mul = modMul

instance ringSignedInt :: Pos b => Ring (SignedInt b) where
  sub = diffFixed

instance baseNSignedInt :: Pos b => BaseN (SignedInt b) where
  toBase r s | isNegative s = "-" <> toBase r (negate s)
  toBase r (SignedInt _ (Bits bits)) = toStringAs r (Bits $ fromMaybe [_0] (A.tail bits))
