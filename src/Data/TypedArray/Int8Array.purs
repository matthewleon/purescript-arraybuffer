module Data.TypedArray.Int8Array (
  fromArray
, fromArrayBuffer
, length
, index
) where

import Data.Array as A
import Data.ArrayBuffer.Types (ArrayBuffer, Int8Array)
import Data.TypedArray as TA
import Data.Maybe (Maybe)
import Prelude ((<<<))
import Unsafe.Coerce (unsafeCoerce)

foreign import fromArray :: Array Int -> Int8Array

foreign import fromArrayBuffer :: ArrayBuffer -> Int8Array

-- TODO: versions of above with arguments

index :: Int8Array -> Int -> Maybe Int
index = A.index <<< unsafeCoerce

length :: Int8Array -> Int
length = TA.length
