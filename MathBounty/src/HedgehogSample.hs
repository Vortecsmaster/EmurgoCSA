{-# language OverloadedStrings #-}

import Hedgehog
import qualified Hedgehog.Gen as Gen 
import qualified Hedgehog.Range as Range
import Test.Tasty
import Tasty.ExpectedFailure
import Test.Tasty.Hedgehog
import Data.ByteString (ByteString)
-- import PlutusLedgerApi.V1 (ScriptContext, StakingCredential)
-- import Data.Maybe (Maybe(Nothing))

-- genAddress :: MonadGen f => Int -> f Address
-- genAddress = Address (PubKeyCredential (genByteString 32)) Nothing

genByteString :: MonadGen f => Int -> f ByteString
genByteString bsl = Gen.bytes (Range.singleton bsl)

genByteStringList :: Gen [ByteString]
genByteStringList =
  Gen.list (Range.linear 0 100) (genByteString 32)

genAlphaList :: Gen String
genAlphaList =
  Gen.list (Range.linear 0 100) Gen.alpha

test_involutive :: (MonadTest m, Eq a, Show a) => (a -> a) -> a -> m ()
test_involutive f x =
  f (f x) === x

prop_reverse_involutive_bytes :: Property
prop_reverse_involutive_bytes =
  property $ do
    xs <- forAll genByteStringList
    classify "empty" $ length xs == 0
    classify "small" $ length xs < 10
    classify "large" $ length xs >= 10
    test_involutive reverse xs

prop_reverse_involutive :: Property
prop_reverse_involutive =
  property $ do
    xs <- forAll genAlphaList
    classify "empty" $ length xs == 0
    classify "small" $ length xs < 10
    classify "large" $ length xs >= 10
    test_involutive reverse xs

badReverse :: [a] -> [a]
badReverse [] = []
badReverse [_] = []
badReverse (x : xs) = badReverse xs ++ [x]

prop_badReverse_involutive :: Property
prop_badReverse_involutive =
  property $ do
    xs <- forAll genAlphaList
    test_involutive badReverse xs

mkValidator :: Int -> () 
mkValidator = if (i < 0) && (-10 <= i) then () else error

main :: IO ()
main = do
  Gen.print genByteStringList

  defaultMain $
    testGroup "tasty-hedgehog tests"
      [ testPropertyNamed
          "reverse involutive"
          "prop_reverse_involutive"
          prop_reverse_involutive
      , testPropertyNamed
          "reverse involutive bytes"
          "prop_reverse_involutive_bytes"
          prop_reverse_involutive_bytes
      , expectFail $ testPropertyNamed
          "badReverse involutive fails"
          "prop_badReverse_involutive"
          prop_badReverse_involutive
      ]