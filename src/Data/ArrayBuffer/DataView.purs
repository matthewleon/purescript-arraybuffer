module Data.ArrayBuffer.DataView (
  Getter
, fromArrayBuffer
, buffer
, byteLength
, byteOffset
, getInt8
, getInt16be
, getInt32be
, getUint8
, getUint16be
, getUint32be
, getFloat32be
, getFloat64be
, getInt16le
, getInt32le
, getUint16le
, getUint32le
, getFloat32le
, getFloat64le
--, getTypedArray
--, getTypedArrayWithLength
) where

import Data.ArrayBuffer.Types (ArrayBuffer, ByteLength, ByteOffset, DataView)
import Data.Function.Uncurried (Fn7, runFn7)
import Data.Maybe (Maybe(..))

-- | Type for all fetching functions.
type Getter r = DataView -> ByteOffset -> Maybe r

-- | View mapping an `ArrayBuffer`.
foreign import fromArrayBuffer :: ArrayBuffer -> DataView

foreign import buffer :: DataView -> ArrayBuffer

foreign import byteLength :: DataView -> Int

foreign import byteOffset :: DataView -> ByteOffset

type Endianness = Boolean

foreign import getterImpl :: forall r. Fn7 (r -> Maybe r) (Maybe r) String ByteLength Endianness DataView ByteOffset (Maybe r)

getter :: forall r. String -> ByteLength -> Endianness -> Getter r
getter = runFn7 getterImpl Just Nothing

-- | Fetch int8 value at a certain index in a `DataView`.
getInt8 :: Getter Int
getInt8 = getter "getInt8" 1 false

-- | Fetch int16 value at a certain index in a `DataView`.
getInt16be :: Getter Int
getInt16be = getter "getInt16" 2 false

getInt16le :: Getter Int
getInt16le = getter "getInt16" 2 true

-- | Fetch int32 value at a certain index in a `DataView`.
getInt32be :: Getter Int
getInt32be = getter "getInt32" 4 false

getInt32le :: Getter Int
getInt32le = getter "getInt32" 4 true

-- | Fetch uint8 value at a certain index in a `DataView`.
getUint8 :: Getter Int
getUint8 = getter "getUint8" 1 false

-- | Fetch uint16 value at a certain index in a `DataView`.
getUint16be :: Getter Int
getUint16be = getter "getUint16" 2 false

getUint16le :: Getter Int
getUint16le = getter "getUint16" 2 true

-- | Fetch uint32 value at a certain index in a `DataView`.
getUint32be :: Getter Int
getUint32be = getter "getUint32" 4 false

getUint32le :: Getter Int
getUint32le = getter "getUint32" 4 true

-- | Fetch float32 value at a certain index in a `DataView`.
getFloat32be :: Getter Number
getFloat32be = getter "getFloat32" 4 false

getFloat32le :: Getter Number
getFloat32le = getter "getFloat32" 4 true

-- | Fetch float64 value at a certain index in a `DataView`.
getFloat64be :: Getter Number
getFloat64be = getter "getFloat64" 8 false

getFloat64le :: Getter Number
getFloat64le = getter "getFloat64" 8 true

{-
getTypedArray :: forall t. Getter (ArrayView t)
getTyepdArray dv bo = buffer dv

getTypedArrayWithLength :: forall t. Int -> Getter (ArrayView t)
-}
