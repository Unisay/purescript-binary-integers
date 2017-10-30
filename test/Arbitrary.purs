module Test.Arbitrary where

import Prelude

import Data.Binary (tryFromInt)
import Data.Binary.SignedInt (SignedInt, fromInt)
import Data.Binary.UnsignedInt (UnsignedInt)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num.Aliases (D31, D32, d32)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (frequency, suchThat)

newtype ArbInt = ArbInt Int
derive newtype instance eqArbInt :: Eq ArbInt
instance arbitraryInt :: Arbitrary ArbInt where
  arbitrary = ArbInt <$> frequency gens where
    gens = Tuple 0.05 (pure 0)      :|
           Tuple 0.05 (pure 1)      :
           Tuple 0.05 (pure (-1))   :
           Tuple 0.05 (pure top)    :
           Tuple 0.05 (pure bottom) :
           Tuple 0.75 arbitrary     :
           Nil

newtype ArbNonNegativeInt = ArbNonNegativeInt Int
instance arbitraryNonNegativeInt :: Arbitrary ArbNonNegativeInt where
  arbitrary = ArbNonNegativeInt <$> frequency gens where
    gens = Tuple 0.05 (pure top)
        :| Tuple 0.05 (pure one)
         : Tuple 0.90 (suchThat arbitrary (_ >= 0))
         : Nil

newtype ArbUnsignedInt31 = ArbUnsignedInt31 (UnsignedInt D31)
derive instance newtypeArbUnsignedInt31 :: Newtype ArbUnsignedInt31 _
instance arbitraryUnsignedInt31 :: Arbitrary ArbUnsignedInt31 where
  arbitrary = do
   (ArbNonNegativeInt i) <- arbitrary
   let ui :: Maybe (UnsignedInt D31)
       ui = tryFromInt i
   pure $ ArbUnsignedInt31 (unsafePartial $ fromJust ui)

newtype ArbSignedInt32 = ArbSignedInt32 (SignedInt D32)
derive instance newtypeArbSignedInt32 :: Newtype ArbSignedInt32 _
instance arbitrarySignedInt32 :: Arbitrary ArbSignedInt32 where
  arbitrary = ArbSignedInt32 <$> fromInt d32 <$> arbitrary


newtype NonOverflowingMultiplicands = NonOverflowingMultiplicands (Tuple Int Int)
instance arbitraryNonOverflowingMultiplicands :: Arbitrary NonOverflowingMultiplicands where
  arbitrary = NonOverflowingMultiplicands <$> (flip suchThat nonOverflowing) do
    (ArbNonNegativeInt a) <- arbitrary
    (ArbNonNegativeInt b) <- arbitrary
    pure (Tuple a b)
    where nonOverflowing (Tuple a b) = (toNumber a) * (toNumber b) <= toNumber (top :: Int)
