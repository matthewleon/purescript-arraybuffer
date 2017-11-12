module Test.Data.ArrayBuffer.TypedArray where

import Prelude

import Data.ArrayBuffer.TypedArray as TA
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

testTypedArray :: Spec (QCRunnerEffects ()) Unit
testTypedArray = describe "TypedArray" do
  describe "eq" $ do
    it "returns true for equal Int32Arrays" $
      quickCheck \xs ->
        TA.fromArray xs `TA.eq` (TA.fromArray xs :: TA.Int32Array)
    it "returns false for unequal Int32Arrays" $
      quickCheck \xs ->
        arbitrary `suchThat` notEq xs <#> \ys ->
          not $ TA.fromArray xs `TA.eq` (TA.fromArray ys :: TA.Int32Array)
  describe "notEq" $ do
    it "returns false for equal Int32Arrays" $
      quickCheck \xs ->
        not $ TA.fromArray xs `TA.notEq` (TA.fromArray xs :: TA.Int32Array)
    it "returns true for unequal Int32Arrays" $
      quickCheck \xs ->
        arbitrary `suchThat` notEq xs <#> \ys ->
          TA.fromArray xs `TA.notEq` (TA.fromArray ys :: TA.Int32Array)
