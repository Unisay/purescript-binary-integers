module Data.SignedInt.Spec
  ( spec
  ) where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (foldr, replicate)
import Data.Array as A
import Data.Binary (toBinString, toInt)
import Data.Binary.SignedInt (fromInt)
import Data.Foldable (all)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Typelevel.Num (d32)
import Test.Arbitrary (ArbInt(..), ArbSignedInt32(ArbSignedInt32))
import Test.QuickCheck (Result(..), (<?>), (===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM, console :: CONSOLE | e)
spec = suite "SignedInt" do
  test "negation" $ quickCheck propNegation
  test "propIntRoundtrip" $ quickCheck propIntRoundtrip
  test "toBinString contains only bin digits" $ quickCheck propBinString
  test "toBinString isn't empty" $ quickCheck propBinStringEmptiness
  test "toBinString produces unique representation" $ quickCheck propBinStringUniqness
  test "addition" $ quickCheck propAddition
  test "multiplication" $ quickCheck propMultiplication

propNegation :: ArbSignedInt32 -> Result
propNegation (ArbSignedInt32 si) =
  si === (foldr compose id (replicate 8 negate) $ si)

propIntRoundtrip :: ArbInt -> Result
propIntRoundtrip (ArbInt i) = i === i' where
  i' = toInt si
  si = fromInt d32 i

propBinString :: ArbSignedInt32 -> Result
propBinString (ArbSignedInt32 ui) =
  let x = toBinString ui
  in all (\d -> d == '1' || d == '0') (Str.toCharArray x)
    <?> "String representation of SignedInt contains not only digits 1 and 0: " <> x

propBinStringEmptiness :: ArbSignedInt32 -> Result
propBinStringEmptiness (ArbSignedInt32 ui) =
  not Str.null (toBinString ui)
    <?> "String representation of SignedInt must not be empty"

propBinStringUniqness :: Array ArbSignedInt32 -> Result
propBinStringUniqness as = A.length sts === A.length uis where
  sts = A.nub $ map toBinString uis
  uis = A.nub $ map unwrap as

propAddition :: ArbInt -> ArbInt -> Result
propAddition (ArbInt a) (ArbInt b) =
  expected == actual
    <?> show a <> " + " <> show b <> " (" <> show expected <> ") "
    <> "/= " <> show (u a) <> " + " <> show (u b)
    <> " (" <> show sum <> ", " <> show actual <> ")"
  where
    expected = a + b
    actual = toInt sum
    sum = u a + u b
    u = fromInt d32

propMultiplication :: ArbInt -> ArbInt -> Result
propMultiplication (ArbInt a) (ArbInt b) | a == top && b == top = Success -- | PS wraps around incorrectly in this case
propMultiplication (ArbInt a) (ArbInt b) =
  expected == actual
    <?> show a <> " * " <> show b <> " (" <> show expected <> ") /= "
    <> show (si a) <> " * " <> show (si b) <> " (" <> show res <> ")"
  where
    actual = toInt res
    expected = a * b
    res = si a * si b
    si = fromInt d32
