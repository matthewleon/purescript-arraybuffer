module Data.ArrayBuffer.DataView.ST (
  STDataView
, Getter
--, Setter
, fromArrayBuffer
, getInt8
, getInt16be
, getInt16le
, getInt32be
, getInt32le
, getUint8
, getUint16be
, getUint16le
, getUint32le
, getUint32be
, getFloat32be
, getFloat32le
, getFloat64be
, getFloat64le
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.ArrayBuffer.ArrayBuffer (ArrayBuffer)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (ByteOffset)
import Data.Maybe (Maybe)
import Data.UInt (UInt)
import Unsafe.Coerce (unsafeCoerce)

foreign import data STDataView :: Type -> Type

-- | Type for all fetching functions.
type Getter a =
    forall h r
   . STDataView h
  -> ByteOffset
  -> Eff (st :: ST h | r) (Maybe a)

foreign import fromArrayBuffer :: forall h r. ArrayBuffer -> Eff (st :: ST h | r) (STDataView h)

getInt8 :: Getter Int
getInt8 = useImmutableGetter DV.getInt8

getInt16be :: Getter Int
getInt16be = useImmutableGetter DV.getInt16be

getInt16le :: Getter Int
getInt16le = useImmutableGetter DV.getInt16le

getInt32be :: Getter Int
getInt32be = useImmutableGetter DV.getInt32be

getInt32le :: Getter Int
getInt32le = useImmutableGetter DV.getInt32le

getUint8 :: Getter UInt
getUint8 = useImmutableGetter DV.getUint8

getUint16be :: Getter UInt
getUint16be = useImmutableGetter DV.getUint16be

getUint16le :: Getter UInt
getUint16le = useImmutableGetter DV.getUint16le

getUint32be :: Getter UInt
getUint32be = useImmutableGetter DV.getUint32be

getUint32le :: Getter UInt
getUint32le = useImmutableGetter DV.getUint32le

getFloat32be :: Getter Number
getFloat32be = useImmutableGetter DV.getFloat32be

getFloat32le :: Getter Number
getFloat32le = useImmutableGetter DV.getFloat32le

getFloat64be :: Getter Number
getFloat64be = useImmutableGetter DV.getFloat64be

getFloat64le :: Getter Number
getFloat64le = useImmutableGetter DV.getFloat64le

useImmutableGetter :: forall a. DV.Getter a -> Getter a
useImmutableGetter g dv = pure <<< g (unsafeCoerce dv)
