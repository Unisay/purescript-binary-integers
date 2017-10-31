module Data.Binary.SignedInt
  ( SignedInt
  , fromInt
  , isNegative
  , complement
  , flipSign
  ) where

import Prelude

import Data.Array as A
import Data.Binary (class Binary, class FitsInt, class Fixed, Bits(Bits), Overflow(Overflow), _0, _1, and, diffFixed, head, modAdd, msb, numBits, or, tail, toStringAs, xor)
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

newtype SignedInt b = SignedInt Bits

instance eqSignedInt :: Pos b => Eq (SignedInt b) where
  eq (SignedInt bits) (SignedInt bits') = eq bits bits'

instance ordSignedInt :: Pos b => Ord (SignedInt b) where
  compare a b | isNegative a && not (isNegative b) = LT
  compare a b | not (isNegative a) && isNegative b = GT
  compare (SignedInt a) (SignedInt b) = compare a b

instance showSignedInt :: Pos b => Show (SignedInt b) where
  show (SignedInt bits) =
    "SignedInt" <> show (Nat.toInt (undefined :: b)) <> "#" <> Bin.toBinString bits

flipSign :: ∀ b. Pos b => SignedInt b -> SignedInt b
flipSign (SignedInt bits) =
  let { head: h, tail: (Bits t) } = Bin.uncons bits
      bs = Bits $ A.cons (Bin.invert h) t
  in SignedInt bs

complement :: ∀ b. Pos b => SignedInt b -> SignedInt b
complement si = (Bin.invert >>> Bin.unsafeAdd _1) si

-- | Converts `Int` value to `SignedInt b` for b >= 31
fromInt :: ∀ b . Pos b => GtEq b D32 => b -> Int -> SignedInt b
fromInt b i = SignedInt signed where
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
isNegative (SignedInt bits) = head bits == _1

instance binarySignedInt :: Pos b => Binary (SignedInt b) where
  _0 = SignedInt _0
  _1 = SignedInt _1
  and (SignedInt as) (SignedInt bs) = SignedInt (and as bs)
  xor (SignedInt as) (SignedInt bs) = SignedInt (xor as bs)
  or  (SignedInt as) (SignedInt bs) = SignedInt (or as bs)
  invert (SignedInt bs) = SignedInt (Bin.invert bs)
  add' bit (SignedInt as) (SignedInt bs) =
    let (Overflow o xs) = Bin.add' bit as bs
    in Overflow (xor o (msb xs)) (SignedInt xs)
  leftShift bit (SignedInt bs) = SignedInt <$> Bin.leftShift bit bs
  rightShift bit (SignedInt bs) = SignedInt <$> Bin.rightShift bit bs
  toBits (SignedInt bs) = bs

instance boundedSignedInt :: Pos b => Bounded (SignedInt b) where
  bottom = SignedInt (Bits (A.cons _1 (A.replicate (Nat.toInt (undefined :: b) - 1) _0)))
  top    = SignedInt (Bits (A.cons _0 (A.replicate (Nat.toInt (undefined :: b) - 1) _1)))

instance fixedSignedInt :: Pos b => Fixed (SignedInt b) where
  numBits _ = Nat.toInt (undefined :: b)
  tryFromBits (Bits bits) = tryFromBits' (A.length bits) (numBits p) bits where
    tryFromBits' len width bs | len == width = Just (SignedInt (Bits bs))
    tryFromBits' _ _ _ = Nothing
    p = Proxy :: Proxy (SignedInt b)

instance fitsIntSignedInt :: (Pos b, LtEq b D32) => FitsInt (SignedInt b) where
  toInt si | si == top = top
  toInt si | si == bottom = bottom
  toInt si@(SignedInt bits) =
    if isNegative si
    then negate let (SignedInt bits') = complement si in abs (tail bits')
    else abs (tail bits)
    where
      -- Safe "by construction"
      abs bs = fromMaybe' (\_ -> unsafeCrashWith err) (Bin.tryToInt bs)
      err = "Failed to convert " <> show si <> " to Int"

signExtend :: Int -> Bits -> Bits
signExtend width bits | Bin.head bits == _0 = Bin.addLeadingZeros width bits
signExtend width (Bits bits) =
  let d = sub width (A.length bits)
  in Bits if d < 1 then bits else (A.replicate d _1) <> bits

instance semiringSignedInt :: Pos b => Semiring (SignedInt b) where
  zero = _0
  add = modAdd
  one = _1
  mul (SignedInt as) (SignedInt bs) = SignedInt (resize prod) where
    resize xs | prodLen < len = Bin.addLeadingZeros len xs
    resize xs | prodLen > len = Bin.drop (prodLen - len) xs
    resize xs = xs
    prodLen = Bin.length prod
    prod = Bin.multiply (signExtend dlen as) (signExtend dlen bs)
    dlen = 2 * len
    len = Nat.toInt (undefined :: b)


instance ringSignedInt :: Pos b => Ring (SignedInt b) where
  sub = diffFixed

instance baseNSignedInt :: Pos b => BaseN (SignedInt b) where
  toBase r s | isNegative s = "-" <> toBase r (negate s)
  toBase r (SignedInt (Bits bits)) = toStringAs r (Bits $ fromMaybe [_0] (A.tail bits))
