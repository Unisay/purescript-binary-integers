module Data.UnsignedInt.Spec
  ( spec
  ) where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Binary (toInt)
import Data.Binary.UnsignedInt (fromInt, toBinString)
import Data.Foldable (all)
import Data.Int as Int
import Data.Newtype (unwrap)
import Data.String (toCharArray, null)
import Data.Typelevel.Num (class GtEq, class Pos, d32, d99)
import Data.Typelevel.Num.Aliases (D31)
import Test.Arbitrary (ArbNonNegativeInt(..), ArbUnsignedInt31(..))
import Test.QuickCheck (Result, (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = suite "UnsignedInt" do
  test "fromInt 32" $ quickCheck (propFromInt d32)
  test "fromInt 99" $ quickCheck (propFromInt d99)
  test "toInt" $ quickCheck propToInt
  test "toBinString contains only bin digits" $ quickCheck propBinString
  test "toBinString isn't empty" $ quickCheck propBinStringEmptiness
  test "toBinString produces unique representation" $ quickCheck propBinStringUniqness


propFromInt :: ∀ b . Pos b => GtEq b D31 =>
               b -> ArbNonNegativeInt -> Result
propFromInt b (ArbNonNegativeInt i) =
  Int.toStringAs Int.binary i === toBinString (fromInt b i)

propToInt :: ArbUnsignedInt31 -> Result
propToInt (ArbUnsignedInt31 ui) =
  toBinString ui === Int.toStringAs Int.binary (toInt ui)

propBinString :: ArbUnsignedInt31 -> Result
propBinString (ArbUnsignedInt31 ui) =
  let x = toBinString ui
  in all (\d -> d == '1' || d == '0') (toCharArray x)
    <?> "String representation of UnsignedInt contains not only digits 1 and 0: " <> x

propBinStringEmptiness :: ArbUnsignedInt31 -> Result
propBinStringEmptiness (ArbUnsignedInt31 ui) =
  not null (toBinString ui)
    <?> "String representation of UnsignedInt must not be empty"

propBinStringUniqness :: Array ArbUnsignedInt31 -> Result
propBinStringUniqness as = A.length sts === A.length uis where
  sts = A.nub $ map toBinString uis
  uis = A.nub $ map unwrap as
