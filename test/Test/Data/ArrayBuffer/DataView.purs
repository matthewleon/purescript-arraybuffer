module Test.Data.ArrayBuffer.DataView where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.TypedArray as TA

testDataView :: forall e. Eff ( console :: CONSOLE | e ) Unit
testDataView = do
  let arr = [100, 101, 102, 103]
      dv  = DV.fromArrayBuffer <<< TA.buffer $ (TA.fromArray arr :: TA.Int8Array)
  logShow arr
  logShow $ DV.getInt8 dv 0
  logShow $ DV.getInt8 dv 1
