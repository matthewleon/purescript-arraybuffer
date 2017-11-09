module Test.Data.ArrayBuffer.DataView.ST where

import Prelude

import Control.Monad.ST (pureST)
import Data.Array as A
import Data.ArrayBuffer.DataView.ST as STDV
import Data.ArrayBuffer.TypedArray as TA
import Data.Maybe (Maybe(..), isJust)
import Test.Data.ArrayBuffer.DataView as TDV
import Test.QuickCheck.Gen (chooseInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

testSTDataView :: Spec (QCRunnerEffects ()) Unit
testSTDataView = describe "STDataView" do
  describe "getInt8" $ do
    it "correctly gets 8-bit integers" $
      quickCheck \(TDV.NonEmptyUntypedInt8Array xs) ->
        let i8a = (TA.fromArray xs) :: TA.Int8Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i ->
              isJust (A.index xs i) &&
              A.index xs i == pureST (
                flip STDV.getInt8 i =<< STDV.fromArrayBuffer (TA.buffer i8a)
              )
    it "returns Nothing for out of range index" $
      quickCheck \(TDV.NonEmptyUntypedInt8Array xs) i ->
        let i8a = (TA.fromArray xs) :: TA.Int8Array
            index = A.length xs + i
        in  A.index xs index == Nothing &&
            Nothing == pureST (
                flip STDV.getInt8 i =<< STDV.fromArrayBuffer (TA.buffer i8a)
              )
    it "returns Nothing for negative index" $
      quickCheck \(TDV.NonEmptyUntypedInt8Array xs) ->
        let i8a = (TA.fromArray xs) :: TA.Int8Array
            index = (-1 - _) <$> chooseInt 0 (A.length xs - 1)
        in  index <#> \i ->
              A.index xs i == Nothing &&
              Nothing == pureST (
                  flip STDV.getInt8 i =<< STDV.fromArrayBuffer (TA.buffer i8a)
              )
