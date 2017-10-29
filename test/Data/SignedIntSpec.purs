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
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d32)
import Test.Arbitrary (ArbInt(..), ArbSignedInt32(ArbSignedInt32), NonOverflowingMultiplicands(NonOverflowingMultiplicands))
import Test.QuickCheck (Result, (<?>), (===))
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

propIntRoundtrip :: Int -> Result
propIntRoundtrip i = i === i' where
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

propAddition :: ArbInt -> ArbInt -> Boolean
propAddition (ArbInt a) (ArbInt b) =
  a + b == toInt (u a + u b) where
  u = fromInt d32

propMultiplication :: NonOverflowingMultiplicands -> Result
propMultiplication (NonOverflowingMultiplicands (Tuple a b)) =
  a * b === toInt (u a * u b) where
  u = fromInt d32
