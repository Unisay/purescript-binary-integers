module Test.Arbitrary where

import Prelude
import Data.Binary.UnsignedInt (UnsignedInt, tryFromInt)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import Data.Typelevel.Num (d31)
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
   pure $ ArbUnsignedInt31 (unsafePartial $ fromJust $ tryFromInt d31 i)
