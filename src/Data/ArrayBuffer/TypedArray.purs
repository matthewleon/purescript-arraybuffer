module Data.ArrayBuffer.TypedArray (
  class IsArrayType
, constructor
, Constructor
, fromArray
, fromArrayBuffer
, fromTypedArray
, buffer
, byteLength
, byteOffset

--
--, toUnfoldable
--, fromFoldable
, empty
, singleton
--, range
--, replicate
--, (..)
--, some
--, many
--, null
, length
--, cons
--, (:)
--, snoc
--, insert
--, head
--, tail
--, init
--, uncons
--, unsnoc
, index
, (!!)
--,elemIndex
--,elemLastIndex
--,findIndex
--,findLastIndex
--,insertAt
--,deleteAt
--,updateAt
--,modifyAt
--,alterAt
--,reverse
--,concat
--,concatMap
, filter
--,partition
--,filterA
--,mapMaybe
--,catMaybes
--,mapWithIndex
--,updateAtIndices
--,modifyAtIndices
--,sort
--,sortBy
--,sortWith
--,slice
--,take
--,takeWhile
--,drop
--,dropWhile
--,span
--,group
--,group'
--,groupBy
--,nub
--,nubBy
--,union
--,unionBy
--,delete
--,deleteBy
--,difference
--,(\\)
--,intersect
--,intersectBy
--,zipWith
--,zipWithA
--,zip
--,unzip
--,foldM
--,foldRecM
, unsafeIndex


-- methods
, every
, map
, foldl
--, bytesPerElement

, module Data.ArrayBuffer.Types
) where

import Data.Array as A
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, ByteOffset, Int8Array, Uint8Array, Float32Array, Int8, Uint8, Float32)
import Data.Maybe (Maybe(..))
import Prelude ((<<<))
import Unsafe.Coerce (unsafeCoerce)

foreign import fromArray :: forall t m. IsArrayType t m => Array m -> t

foreign import fromArrayBuffer :: forall t m. IsArrayType t m => ArrayBuffer -> t

fromArrayBufferWithOffset
  :: forall t m
   . IsArrayType t m
  => ArrayBuffer
  -> ByteOffset
  -> Maybe t
fromArrayBufferWithOffset = fromArrayBufferWithOffsetImpl Just Nothing

foreign import fromArrayBufferWithOffsetImpl
  :: forall t m
   . IsArrayType t m
  => (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> ArrayBuffer
  -> ByteOffset
  -> Maybe t

fromArrayBufferWithOffsetAndLength
  :: forall t m
   . IsArrayType t m
  => ArrayBuffer
  -> ByteOffset
  -> Int
  -> Maybe t
fromArrayBufferWithOffsetAndLength =
  fromArrayBufferWithOffsetAndLengthImpl Just Nothing

foreign import fromArrayBufferWithOffsetAndLengthImpl
  :: forall t m
   . IsArrayType t m
  => (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> ArrayBuffer
  -> ByteOffset
  -> Int
  -> Maybe t

foreign import fromTypedArray :: forall t t'. ArrayView t -> ArrayView t'

foreign import buffer :: forall t. ArrayView t -> ArrayBuffer

foreign import byteLength :: forall t. ArrayView t -> Int

foreign import byteOffset :: forall t. ArrayView t -> Int

foreign import length :: forall t. ArrayView t -> Int

-- | Create an empty typed array.
foreign import empty :: forall t m. IsArrayType t m => t

-- | Create a typed array of one element
singleton :: forall t m. IsArrayType t m => m -> t
singleton = fromArray <<< A.singleton

index :: forall t m. IsArrayType t m => t -> Int -> Maybe m
index = A.index <<< unsafeCoerce

-- | An infix version of `index`.
infixl 8 index as !!

foreign import every :: forall t m. IsArrayType t m => (m -> Boolean) -> t -> Boolean

foreign import filter :: forall t m. IsArrayType t m => (m -> Boolean) -> t -> t

foreign import map :: forall t m. IsArrayType t m => (m -> m) -> t -> t

foreign import foldl :: forall t m a. IsArrayType t m => (a -> m -> a) -> a -> t -> a

-- | Find the element of an array at the specified index.
unsafeIndex :: forall t m. IsArrayType t m => Partial => t -> Int -> m
unsafeIndex = unsafeIndexImpl

foreign import unsafeIndexImpl :: forall t m. IsArrayType t m => t -> Int -> m

class IsArrayType t m | t -> m where
  constructor :: Constructor t

data Constructor t

-- TODO
--foreign import bytesPerElement :: forall t m. IsArrayType t m => Int

instance arrayTypeInt8Array :: IsArrayType (ArrayView Int8) Int
  where constructor = int8ArrayConstructor
instance arrayTypeUint8Array :: IsArrayType (ArrayView Uint8) Int
  where constructor = uint8ArrayConstructor

foreign import int8ArrayConstructor :: Constructor Int8Array
foreign import uint8ArrayConstructor :: Constructor Uint8Array
foreign import float32ArrayConstructor :: Constructor Float32Array
