module Test.Data.ArrayBuffer.DataView where

import Prelude

import Data.Array as A
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.TypedArray as TA
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty (NonEmpty)
import Data.Ord (abs)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

testDataView :: Spec (QCRunnerEffects ()) Unit
testDataView = describe "DataView" do
  describe "getInt8" $ do
    it "correctly gets 8-bit integers" $
      quickCheck \(NonEmptyUntypedInt8Array xs) ->
        let i8a = (TA.fromArray xs) :: TA.Int8Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i ->
              isJust (A.index xs i) &&
              A.index xs i == DV.getInt8 (DV.fromArrayBuffer $ TA.buffer i8a) i
    it "returns Nothing for out of range index" $
      quickCheck \xs i ->
        let i8a = (TA.fromArray xs) :: TA.Int8Array
            index = A.length xs + i
        in  A.index xs index == Nothing &&
            DV.getInt8 (DV.fromArrayBuffer $ TA.buffer i8a) index == Nothing

newtype NonEmptyUntypedInt8Array = NonEmptyUntypedInt8Array (Array Int)
instance arbitraryNonEmptyUntypedInt8Array :: Arbitrary NonEmptyUntypedInt8Array where
  arbitrary = NonEmptyUntypedInt8Array <<< 
    map intToInt8 <$> A.fromFoldable <$> (arbitrary :: Gen (NonEmpty Array Int))
    where intToInt8 x = 127 - (abs x `mod` 256)
