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
    describe "Int8Array" do
      it "returns true when equal" $
        quickCheck \xs ->
          TA.fromArray xs `TA.eq` (TA.fromArray xs :: TA.Int8Array)
      it "returns false when unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            not $ TA.fromArray xs `TA.eq` (TA.fromArray ys :: TA.Int8Array)
    describe "Int16Array" do
      it "returns true when equal" $
        quickCheck \xs ->
          TA.fromArray xs `TA.eq` (TA.fromArray xs :: TA.Int16Array)
      it "returns false when unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            not $ TA.fromArray xs `TA.eq` (TA.fromArray ys :: TA.Int16Array)
    describe "Int32Array" do
      it "returns true when equal" $
        quickCheck \xs ->
          TA.fromArray xs `TA.eq` (TA.fromArray xs :: TA.Int32Array)
      it "returns false when unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            not $ TA.fromArray xs `TA.eq` (TA.fromArray ys :: TA.Int32Array)
  describe "notEq" $ do
    describe "Int8Array" do
      it "returns false for equal" $
        quickCheck \xs ->
          not $ TA.fromArray xs `TA.notEq` (TA.fromArray xs :: TA.Int8Array)
      it "returns true for unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            TA.fromArray xs `TA.notEq` (TA.fromArray ys :: TA.Int8Array)
    describe "Int16Array" do
      it "returns false for equal" $
        quickCheck \xs ->
          not $ TA.fromArray xs `TA.notEq` (TA.fromArray xs :: TA.Int16Array)
      it "returns true for unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            TA.fromArray xs `TA.notEq` (TA.fromArray ys :: TA.Int16Array)
    describe "Int32Array" do
      it "returns false for equal" $
        quickCheck \xs ->
          not $ TA.fromArray xs `TA.notEq` (TA.fromArray xs :: TA.Int32Array)
      it "returns true for unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            TA.fromArray xs `TA.notEq` (TA.fromArray ys :: TA.Int32Array)
