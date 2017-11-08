module Test.Data.ArrayBuffer.DataView where

import Prelude

import Data.Array as A
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.TypedArray as TA
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty (NonEmpty)
import Data.Ord (abs)
import Data.UInt as U
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
      quickCheck \(NonEmptyUntypedInt8Array xs) i ->
        let i8a = (TA.fromArray xs) :: TA.Int8Array
            index = A.length xs + i
        in  A.index xs index == Nothing &&
            DV.getInt8 (DV.fromArrayBuffer $ TA.buffer i8a) index == Nothing

  describe "getInt16le" $ do
    it "correctly gets 16-bit integers" $
      quickCheck \(NonEmptyUntypedInt16Array xs) ->
        let i16a = (TA.fromArray xs) :: TA.Int16Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i -> 
              let byteIndex = i * 2
              in  isJust (A.index xs i)
                  && A.index xs i
                     == DV.getInt16le (DV.fromArrayBuffer $ TA.buffer i16a) byteIndex
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedInt16Array xs) i ->
        let i16a = (TA.fromArray xs) :: TA.Int16Array
            index = A.length xs + i
            byteIndex = index * 2
        in  A.index xs index == Nothing
            && DV.getInt16le (DV.fromArrayBuffer $ TA.buffer i16a) byteIndex
               == Nothing

  describe "getInt16be" $ do
    it "correctly gets 16-bit integers" $
      quickCheck \(NonEmptyUntypedInt16Array xs) ->
        let i16a = (TA.fromArray xs) :: TA.Int16Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i ->
              let byteIndex = i * 2
                  byteFlippedFromArray =
                    flip DV.getInt16be 0
                    =<< DV.fromArrayBuffer <<< TA.buffer
                    <$> ((TA.fromArray <<< A.singleton <$> A.index xs i)
                         :: Maybe TA.Int16Array)
              in isJust byteFlippedFromArray
                 && byteFlippedFromArray
                    == DV.getInt16be (DV.fromArrayBuffer $ TA.buffer i16a) byteIndex
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedInt16Array xs) i ->
        let i16a = (TA.fromArray xs) :: TA.Int16Array
            index = A.length xs + i
            byteIndex = index * 2
        in  A.index xs index == Nothing
            && DV.getInt16be (DV.fromArrayBuffer $ TA.buffer i16a) byteIndex
               == Nothing

  describe "getInt32le" $ do
    it "correctly gets 32-bit integers" $
      quickCheck \(NonEmptyUntypedInt32Array xs) ->
        let i32a = (TA.fromArray xs) :: TA.Int32Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i -> 
              let byteIndex = i * 4
              in  isJust (A.index xs i)
                  && A.index xs i
                     == DV.getInt32le (DV.fromArrayBuffer $ TA.buffer i32a) byteIndex
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedInt32Array xs) i ->
        let i32a = (TA.fromArray xs) :: TA.Int32Array
            index = A.length xs + i
            byteIndex = index * 4
        in  A.index xs index == Nothing
            && DV.getInt32le (DV.fromArrayBuffer $ TA.buffer i32a) byteIndex
               == Nothing

  describe "getInt32be" $ do
    it "correctly gets 32-bit integers" $
      quickCheck \(NonEmptyUntypedInt32Array xs) ->
        let i32a = (TA.fromArray xs) :: TA.Int32Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i ->
              let byteIndex = i * 4
                  byteFlippedFromArray =
                    flip DV.getInt32be 0
                    =<< DV.fromArrayBuffer <<< TA.buffer
                    <$> ((TA.fromArray <<< A.singleton <$> A.index xs i)
                         :: Maybe TA.Int32Array)
              in isJust byteFlippedFromArray
                 && byteFlippedFromArray
                    == DV.getInt32be (DV.fromArrayBuffer $ TA.buffer i32a) byteIndex
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedInt32Array xs) i ->
        let i32a = (TA.fromArray xs) :: TA.Int32Array
            index = A.length xs + i
            byteIndex = index * 4
        in  A.index xs index == Nothing
            && DV.getInt32be (DV.fromArrayBuffer $ TA.buffer i32a) byteIndex
               == Nothing
               
  describe "getUint8" $ do
    it "correctly gets 8-bit unsigned integers" $
      quickCheck \(NonEmptyUntypedUint8Array xs) ->
        let u8a = (TA.fromArray xs) :: TA.Uint8Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i ->
              isJust (A.index xs i) &&
              A.index xs i == DV.getUint8 (DV.fromArrayBuffer $ TA.buffer u8a) i
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedUint8Array xs) i ->
        let u8a = (TA.fromArray xs) :: TA.Uint8Array
            index = A.length xs + i
        in  A.index xs index == Nothing &&
            DV.getUint8 (DV.fromArrayBuffer $ TA.buffer u8a) index == Nothing

  describe "getUint16le" $ do
    it "correctly gets 16-bit unsigned integers" $
      quickCheck \(NonEmptyUntypedUint16Array xs) ->
        let u16a = (TA.fromArray xs) :: TA.Uint16Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i -> 
              let byteIndex = i * 2
              in  isJust (A.index xs i)
                  && A.index xs i
                     == DV.getUint16le (DV.fromArrayBuffer $ TA.buffer u16a) byteIndex
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedUint16Array xs) i ->
        let u16a = (TA.fromArray xs) :: TA.Uint16Array
            index = A.length xs + i
            byteIndex = index * 2
        in  A.index xs index == Nothing
            && DV.getUint16le (DV.fromArrayBuffer $ TA.buffer u16a) byteIndex
               == Nothing

  describe "getUInt16be" $ do
    it "correctly gets 16-bit unsigned integers" $
      quickCheck \(NonEmptyUntypedUint16Array xs) ->
        let u16a = (TA.fromArray xs) :: TA.Uint16Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i ->
              let byteIndex = i * 2
                  byteFlippedFromArray =
                    flip DV.getUint16be 0
                    =<< DV.fromArrayBuffer <<< TA.buffer
                    <$> ((TA.fromArray <<< A.singleton <$> A.index xs i)
                         :: Maybe TA.Uint16Array)
              in isJust byteFlippedFromArray
                 && byteFlippedFromArray
                    == DV.getUint16be (DV.fromArrayBuffer $ TA.buffer u16a) byteIndex
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedUint16Array xs) i ->
        let u16a = (TA.fromArray xs) :: TA.Uint16Array
            index = A.length xs + i
            byteIndex = index * 2
        in  A.index xs index == Nothing
            && DV.getUint16be (DV.fromArrayBuffer $ TA.buffer u16a) byteIndex
               == Nothing

  describe "getUint32le" $ do
    it "correctly gets 32-bit unsigned integers" $
      quickCheck \(NonEmptyUntypedUint32Array xs) ->
        let u32a = (TA.fromArray xs) :: TA.Uint32Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i -> 
              let byteIndex = i * 4
              in  isJust (A.index xs i)
                  && A.index xs i
                     == DV.getUint32le (DV.fromArrayBuffer $ TA.buffer u32a) byteIndex
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedUint32Array xs) i ->
        let u32a = (TA.fromArray xs) :: TA.Uint32Array
            index = A.length xs + i
            byteIndex = index * 4
        in  A.index xs index == Nothing
            && DV.getUint32le (DV.fromArrayBuffer $ TA.buffer u32a) byteIndex
               == Nothing

  describe "getUInt32be" $ do
    it "correctly gets 32-bit unsigned integers" $
      quickCheck \(NonEmptyUntypedUint32Array xs) ->
        let u32a = (TA.fromArray xs) :: TA.Uint32Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i ->
              let byteIndex = i * 4
                  byteFlippedFromArray =
                    flip DV.getUint32be 0
                    =<< DV.fromArrayBuffer <<< TA.buffer
                    <$> ((TA.fromArray <<< A.singleton <$> A.index xs i)
                         :: Maybe TA.Uint32Array)
              in isJust byteFlippedFromArray
                 && byteFlippedFromArray
                    == DV.getUint32be (DV.fromArrayBuffer $ TA.buffer u32a) byteIndex
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedUint32Array xs) i ->
        let u32a = (TA.fromArray xs) :: TA.Uint32Array
            index = A.length xs + i
            byteIndex = index * 4
        in  A.index xs index == Nothing
            && DV.getUint32be (DV.fromArrayBuffer $ TA.buffer u32a) byteIndex
               == Nothing

  describe "getFloat32le" $ do
    it "correctly gets 32-bit float" $
      quickCheck \(NonEmptyUntypedNumberArray xs) ->
        let f32a = (TA.fromArray xs) :: TA.Float32Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i ->
              let byteIndex = i * 4
                  float32leFromArray =
                    flip DV.getFloat32le 0
                    =<< DV.fromArrayBuffer <<< TA.buffer
                    <$> ((TA.fromArray <<< A.singleton <$> A.index xs i)
                         :: Maybe TA.Float32Array)
              in isJust float32leFromArray
                 && float32leFromArray
                    == DV.getFloat32le (DV.fromArrayBuffer $ TA.buffer f32a) byteIndex
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedUint32Array xs) i ->
        let f32a = (TA.fromArray xs) :: TA.Uint32Array
            index = A.length xs + i
            byteIndex = index * 4
        in  A.index xs index == Nothing
            && DV.getUint32be (DV.fromArrayBuffer $ TA.buffer f32a) byteIndex
               == Nothing

  describe "getFloat32be" $ do
    it "correctly gets 32-bit float" $
      quickCheck \(NonEmptyUntypedNumberArray xs) ->
        let f32a = (TA.fromArray xs) :: TA.Float32Array
            index = chooseInt 0 (A.length xs - 1)
        in  index <#> \i ->
              let byteIndex = i * 4
                  float32beFromArray =
                    flip DV.getFloat32be 0
                    =<< DV.fromArrayBuffer <<< TA.buffer
                    <$> ((TA.fromArray <<< A.singleton <$> A.index xs i)
                         :: Maybe TA.Float32Array)
              in isJust float32beFromArray
                 && float32beFromArray
                    == DV.getFloat32be (DV.fromArrayBuffer $ TA.buffer f32a) byteIndex
    it "returns Nothing for out of range index" $
      quickCheck \(NonEmptyUntypedUint32Array xs) i ->
        let f32a = (TA.fromArray xs) :: TA.Uint32Array
            index = A.length xs + i
            byteIndex = index * 4
        in  A.index xs index == Nothing
            && DV.getUint32be (DV.fromArrayBuffer $ TA.buffer f32a) byteIndex
               == Nothing

newtype NonEmptyUntypedInt8Array = NonEmptyUntypedInt8Array (Array Int)
instance arbitraryNonEmptyUntypedInt8Array :: Arbitrary NonEmptyUntypedInt8Array where
  arbitrary = NonEmptyUntypedInt8Array <<<
    map intToInt8 <$> A.fromFoldable <$> (arbitrary :: Gen (NonEmpty Array Int))
    where intToInt8 x = 127 - (abs x `mod` 256)

newtype NonEmptyUntypedInt16Array = NonEmptyUntypedInt16Array (Array Int)
instance arbitraryNonEmptyUntypedInt16Array :: Arbitrary NonEmptyUntypedInt16Array where
  arbitrary = NonEmptyUntypedInt16Array <<<
    map intToInt16 <$> A.fromFoldable <$> (arbitrary :: Gen (NonEmpty Array Int))
    where intToInt16 x = 32767 - (abs x `mod` 65536)

newtype NonEmptyUntypedInt32Array = NonEmptyUntypedInt32Array (Array Int)
instance arbitraryNonEmptyUntypedInt32Array :: Arbitrary NonEmptyUntypedInt32Array where
  arbitrary = NonEmptyUntypedInt32Array <<< A.fromFoldable
    <$> (arbitrary :: Gen (NonEmpty Array Int))

newtype NonEmptyUntypedUint8Array = NonEmptyUntypedUint8Array (Array U.UInt)
instance arbitraryNonEmptyUntypedUint8Array :: Arbitrary NonEmptyUntypedUint8Array where
  arbitrary = NonEmptyUntypedUint8Array <<<
    map intToUint8 <$> A.fromFoldable <$> (arbitrary :: Gen (NonEmpty Array Int))
    where intToUint8 x = U.fromInt x `mod` U.fromInt 256

newtype NonEmptyUntypedUint16Array = NonEmptyUntypedUint16Array (Array U.UInt)
instance arbitraryNonEmptyUntypedUint16Array :: Arbitrary NonEmptyUntypedUint16Array where
  arbitrary = NonEmptyUntypedUint16Array <<<
    map intToUint16 <$> A.fromFoldable <$> (arbitrary :: Gen (NonEmpty Array Int))
    where intToUint16 x = U.fromInt x `mod` U.fromInt 65536

newtype NonEmptyUntypedUint32Array = NonEmptyUntypedUint32Array (Array U.UInt)
instance arbitraryNonEmptyUntypedUint32Array :: Arbitrary NonEmptyUntypedUint32Array where
  arbitrary = NonEmptyUntypedUint32Array <<<
    map U.fromInt <$> A.fromFoldable <$> (arbitrary :: Gen (NonEmpty Array Int))

newtype NonEmptyUntypedNumberArray = NonEmptyUntypedNumberArray (Array Number)
instance arbitraryNonEmptyUntypedNumberArray :: Arbitrary NonEmptyUntypedNumberArray where
  arbitrary = NonEmptyUntypedNumberArray <<<
    A.fromFoldable <$> (arbitrary :: Gen (NonEmpty Array Number))
