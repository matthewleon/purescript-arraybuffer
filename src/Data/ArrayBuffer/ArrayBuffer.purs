module Data.ArrayBuffer.ArrayBuffer (
  byteLength
, slice
, module Data.ArrayBuffer.Types
) where

import Data.Function.Uncurried (Fn3, runFn3)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)

-- | Represents the length of an `ArrayBuffer` in bytes.
foreign import byteLength :: ArrayBuffer -> ByteLength

foreign import sliceImpl :: Fn3 ByteOffset Int ArrayBuffer ArrayBuffer

-- | Returns a new `ArrayBuffer` whose contents are a copy of this ArrayBuffer's bytes from begin, inclusive, up to end, exclusive.
slice :: ByteOffset -> Int -> ArrayBuffer -> ArrayBuffer
slice = runFn3 sliceImpl
