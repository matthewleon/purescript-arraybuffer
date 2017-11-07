module Data.ArrayBuffer.TypedArray.Class (
  class IsArrayType
, Constructor
, constructor
) where

import Data.ArrayBuffer.Types (ArrayView, Int8Array, Uint8Array, Int16Array, Uint16Array, Int32Array, Uint32Array, Float32Array, Int8, Uint8, Int16, Uint16, Int32, Uint32, Float32)
import Data.UInt (UInt)

class IsArrayType t m | t -> m where
  constructor :: Constructor t

data Constructor t

instance arrayTypeInt8Array :: IsArrayType (ArrayView Int8) Int
  where constructor = int8ArrayConstructor
instance arrayTypeUint8Array :: IsArrayType (ArrayView Uint8) UInt
  where constructor = uint8ArrayConstructor
instance arrayTypeInt16Array :: IsArrayType (ArrayView Int16) Int
  where constructor = int16ArrayConstructor
instance arrayTypeUint16Array :: IsArrayType (ArrayView Uint16) UInt
  where constructor = uint16ArrayConstructor
instance arrayTypeInt32Array :: IsArrayType (ArrayView Int32) Int
  where constructor = int32ArrayConstructor
instance arrayTypeUint32Array :: IsArrayType (ArrayView Uint32) UInt
  where constructor = uint32ArrayConstructor
instance arrayTypeFloat32Array :: IsArrayType (ArrayView Float32) Number
  where constructor = float32ArrayConstructor

foreign import int8ArrayConstructor :: Constructor Int8Array
foreign import uint8ArrayConstructor :: Constructor Uint8Array
foreign import int16ArrayConstructor :: Constructor Int16Array
foreign import uint16ArrayConstructor :: Constructor Uint16Array
foreign import int32ArrayConstructor :: Constructor Int32Array
foreign import uint32ArrayConstructor :: Constructor Uint32Array
foreign import float32ArrayConstructor :: Constructor Float32Array
