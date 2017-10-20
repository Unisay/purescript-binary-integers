module Data.Binary.UnsignedInt
  ( UnsignedInt
  , fromInt
  ) where

import Prelude

import Data.Binary (class Binary, class FitsInt, class Fixed, _0, _1, bitsLength, double, half, isOdd, numBits, unsafeAdd)
import Data.Binary as Bin
import Data.Bits (Bits)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe')
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class GtEq, class Lt, D32)
import Data.Typelevel.Num as Nat
import Data.Typelevel.Num.Aliases (D31)
import Data.Typelevel.Num.Sets (class Pos)
import Data.Typelevel.Undefined (undefined)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))


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

instance binaryUnsignedInt :: Pos b => Binary (UnsignedInt b) where
  _0 = UnsignedInt undefined _0
  _1 = UnsignedInt undefined _1
  invert (UnsignedInt b bs) = UnsignedInt b (Bin.invert bs)
  add' bit (UnsignedInt b as) (UnsignedInt _ bs) = UnsignedInt b <$> Bin.add' bit as bs
  leftShift bit (UnsignedInt b bs) = UnsignedInt b <$> Bin.leftShift bit bs
  rightShift bit (UnsignedInt b bs) = UnsignedInt b <$> Bin.rightShift bit bs
  toBits (UnsignedInt b bs) = bs

instance fixedUnsignedInt :: Pos b => Fixed (UnsignedInt b) where
  numBits _ = Nat.toInt (undefined :: b)
  tryFromBits bits =
    if bitsLength bits <= numBits p
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


{-



multiply :: ∀ a. Binary a => a -> a -> Overflow (Maybe Bits) a
multiply md mr = foldl g (Overflow Nothing) partialProducts where
  g :: Overflow (Maybe Bits) a -> Bits -> Overflow (Maybe Bits) a
  g = unsafeCoerce unit
  partialProducts :: Array Bits
  partialProducts = A.filter lastBitIs1 (unfoldr f initialState)
  lastBitIs1 :: Bits -> Boolean
  lastBitIs1 (Bits (NonEmpty _ bits)) = maybe false (eq _1) (A.last bits)
  initialState :: Tuple (Array Bit) Bits
  initialState = Tuple (bitsArray $ toBits md) (toBits mr)
  f :: Tuple (Array Bit) Bits -> Maybe (Tuple Bits (Tuple (Array Bit) Bits))
  f (Tuple [] mdr) = Nothing
  f (Tuple mdb mdr) = A.tail mdb <#> \shr -> (Tuple shifted (Tuple shr shifted))
     where shifted = mdr <> _0
-}
