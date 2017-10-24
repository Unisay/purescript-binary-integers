module Test.Arbitrary where

import Prelude

import Data.Binary (tryFromInt)
import Data.Binary.UnsignedInt (UnsignedInt)
import Data.Int (toNumber)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num.Aliases (D31)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)

newtype ArbNonNegativeInt = ArbNonNegativeInt Int

instance arbitraryNonNegativeInt :: Arbitrary ArbNonNegativeInt where
  arbitrary = ArbNonNegativeInt <$> suchThat arbitrary (_ >= 0)

newtype ArbUnsignedInt31 = ArbUnsignedInt31 (UnsignedInt D31)
derive instance newtypeArbUnsignedInt31 :: Newtype ArbUnsignedInt31 _
instance arbitraryUnsignedInt31 :: Arbitrary ArbUnsignedInt31 where
  arbitrary = do
   (ArbNonNegativeInt i) <- arbitrary
   let ui :: Maybe (UnsignedInt D31)
       ui = tryFromInt i
   pure $ ArbUnsignedInt31 (unsafePartial $ fromJust ui)

newtype NonOverflowingMultiplicands = NonOverflowingMultiplicands (Tuple Int Int)
instance arbitraryNonOverflowingMultiplicands :: Arbitrary NonOverflowingMultiplicands where
  arbitrary = NonOverflowingMultiplicands <$> (flip suchThat nonOverflowing) do
    (ArbNonNegativeInt a) <- arbitrary
    (ArbNonNegativeInt b) <- arbitrary
    pure (Tuple a b)
    where nonOverflowing (Tuple a b) = (toNumber a) * (toNumber b) <= toNumber (top :: Int)
